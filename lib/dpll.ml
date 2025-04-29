module A = Ast
module SA = SygusAst

(*
A -> B C { B.F < C.J } | D E
B -> F G | H I
C -> J K | L M
_ :: Int
*)

(* 
  TODO:
  * Make sure paths are unique
  * Handle dependent terms (later in pipeline)
  * Backtracking
  * Make sure ambiguous references handled properly
*)

(* 
  * At each production rule, choose an option to pursue 
  * Keep in context a set of constraints wrt the current root 
  * Construct a derivation tree, depth first, DPLL-style
  * When you get to a nonterminal
    * If it isn't constrained, instantiate it
    * If it is constrained and not tied to other terminals, instantiate w/ SMT solver
    * If it is constrained and is tied to other terminals, expand the derivation 
      tree until you reach the other terminals, and instantiate them all together
    * When determining if a terminal is "tied to" another terminal, you have to 
      consider recursive dependencies

<S> -> terminal | <S> <S>



<S> -> <S> <A> { <S>.<A> > 0; } | <A>
<A> -> ...

<A> -> ... 

<S> -> <S>* | terminal

<S> -> <A> 
<A> -> <S> | ... 


<S> -> <A> { <A>.<B> > 0; }; 
-- <A>.<B> may or may not need to be pushed depending on the path
-- Look at CLP
<A> -> <B> | <C>
...

Notes
  * Don't need the restriction on grammar recursiveness in the direct approach
  * Don't need a recursive function for constraints; hold and instantiate
  * Conceptually similar to datalog/CLP, but with SMT constraint solving
  * Lean into incremental nature of the solver; extra work is proportional to new constraint,
    not the whole problem
  * Optimization: minimize grammar (explore later as needed)

Ideas to implement
  * Choosing which production rule option to expand
    * Depth-first, left-first
    * Coin flip. Better (in terms of randomness of generated terms), 
      but then you need to track visited paths.
    * IDS
  * Backtracking style 
    * Fail on UNSAT
    * If you hit UNSAT, backtrack chronologically DPLL-style
    * If you hit UNSAT, backjump CDCL-style (using the UNSAT cores)
  * Decomposing the problem
    * Monolithic---expand all the way to terminals, 
      (optionally) divide into separate queries based on shared variables, 
      and go
    * Incremental---push constraints as you hit them. Allows you to backtrack 
      sooner in the case of an unsatisfiable path. But maybe 
      the paths won't be unsatisfiable (in practice) for fuzzing contexts. Also, 
      won't you potentially push constraints that relate to derivation paths 
      (production rule options) you won't take?
    * Compositional reasoning: Solve subproblems independently, bottom up, 
      and ask for new solutions only if needed. 
      But, this leads us to a similar position as ISLa, where the solutions to subproblems 
      may not fit in with the global solution
    * Somehow decompose the problem based on interacting variables. 
      Similar to how overlapping semantic constraints are handled
  * Another level--should we divide-and-conquer exactly as we did for sygus? 
    or not at all? Or some compromise? 
    * Could divide all the way 
    * Could do no dividing
    * Could do some compromise

Naive strategy

Instantiating a strategy
1) Depth-first, left-first (kind of, see 3)
2) Fail on UNSAT
3) 
  * Keep a derivation tree so far
  * Keep a frontier of nodes to explore
  * Keep disjoint sets of constraints (merge if they have overlapping terminals)
  * For each set of constraints, keep a queue of terminals to explore to. 
    You can dequeue a terminal if either (1) you reach it, or (2) it becomes unreachable in the derivation tree. 
  * You can solve and instantiate a set of constraints when its queue becomes empty
The idea is to avoid backtracking as much as possible; wait until all needed nonterminals 
are on the frontier before solving. But also avoid monolithic solving. 
4) High level
  * Keep a derivation tree
  * Iteratively keep expanding it depth-first
    * If you hit a constraint, use the set of terminals it references to add it 
      to the disjoint sets (of constraints)
    * If you make a decision, remove the corresponding constraints and terminals from the 
      disjoint sets
  * When you hit a non-terminal, check if it is referenced by any constraint set
    * If not, instantiate it
    * If it is the last unexplored nonterminal referenced by the constraint set, 
      then solve + instantiate
    * If it is one of multiple unexplored nonterminals, then leave it abstract,
      remove it from the nonterminals referenced by the constraint set, and move on

Interfacing with the solver
  * One strategy
    * Flatten each NT name, all the way from root, with indices to disambiguate. 
      E.g., root.nt1[0].nt2[1].leaf]
    * Parse the model into an AST (with each new variable, incrementally expand the tree)
    * Serialize the AST
*)

(* Module state *)
let i = ref 0
let assertion_level = ref 1

type solver_instance = {
  in_channel : in_channel;   
  out_channel : out_channel; 
  err_channel : in_channel;  
}

type model_value = 
| ConcreteInt of int

type derivation_tree = 
| SymbolicIntLeaf of string list (* path to this node *)
| ConcreteIntLeaf of string list * int (* path to this node, value of the leaf *)
(* label, index, path to this node, children *)
(* The index is of which production rule option you chose, to enable backtracking *)
| Node of string * int option ref * string list * derivation_tree list ref

module DTSet = Set.Make(struct
  type t = derivation_tree ref
  let compare = Stdlib.compare
end)

let random_int_in_range: int -> int -> int
= fun min max ->
  min + Random.int (max - min + 1) 

let issue_solver_command: string -> solver_instance -> unit 
= fun cmd_string solver -> 
  Format.fprintf Format.std_formatter "Issuing %s\n" 
    cmd_string;
  output_string solver.out_channel cmd_string;
  flush solver.out_channel

let declare_smt_variables: Utils.StringSet.t ref -> A.il_type Utils.StringMap.t -> solver_instance -> unit 
= fun declared_variables ctx solver -> 
  Utils.StringMap.iter (fun var ty -> 
    if Utils.StringSet.mem var !declared_variables then 
      () 
    else 
      let declaration_string = Format.asprintf "(declare-fun %s () %a)\n" var A.pp_print_ty ty in
      declared_variables := Utils.StringSet.add var !declared_variables;
      issue_solver_command declaration_string solver
  ) ctx 

let read_get_model_response solver =
  let rec loop acc parens =
    try
      let line = input_line solver.in_channel in
      let parens = parens + (List.length (String.split_on_char '(' line) - 1) - (List.length (String.split_on_char ')' line) - 1) in
      let acc = acc ^ "\n" ^ line in
      if parens <= 0 && (not (String.equal line "sat")) then
        acc
      else
        loop acc parens
    with End_of_file -> 
      acc
  in
  let result = (loop "" 0) in
  result
  

let initialize_cvc5 () : solver_instance =
  let cvc5 = Utils.find_command_in_path "cvc5" in
  let cmd = 
    Printf.sprintf "%s --produce-models --dag-thresh=0 --lang=smtlib2 --incremental" 
      cvc5 
  in
  let (in_chan, out_chan, err_chan) = Unix.open_process_full cmd (Unix.environment ()) in
  let set_logic_command = Format.asprintf "(set-logic QF_LIA)\n" in
  let solver = { in_channel = in_chan; out_channel = out_chan; err_channel = err_chan } in
  issue_solver_command set_logic_command solver;
  solver

(* TODO: Check result for unsat and backtrack if necessary *)
let assert_smt_constraint: string -> solver_instance -> Ast.expr -> unit 
= fun nt_prefix solver expr ->
  let push_cmd = Format.asprintf "(push %d)" !assertion_level in
  let assert_cmd = 
    Format.asprintf "(assert %a)\n" (Sygus.pp_print_expr ~nt_prefix:nt_prefix Utils.StringMap.empty) expr 
  in
  assertion_level := !assertion_level + 1;
  issue_solver_command push_cmd solver; 
  issue_solver_command assert_cmd solver; 
  ()

let rec model_of_sygus_ast: SygusAst.sygus_ast -> model_value Utils.StringMap.t 
= fun sygus_ast -> 
  let r = model_of_sygus_ast in 
  match sygus_ast with 
  | VarLeaf _ 
  | BLLeaf _ 
  | BVLeaf _ -> Utils.crash "Unsupported case in model_of_sygus_ast"
  | Node (constructor, [IntLeaf value]) -> 
    Utils.StringMap.singleton constructor (ConcreteInt value)
  | Node (_, children) -> 
    List.fold_left (fun acc child -> 
      let map = r child in 
      Utils.StringMap.merge Lib.union_keys acc map  
    ) Utils.StringMap.empty children
  | IntLeaf _ -> Utils.crash "Unexpected case in model_of_sygus_ast"

let get_smt_result: A.ast -> solver_instance -> model_value Utils.StringMap.t 
= fun ast solver -> 
  issue_solver_command "(check-sat)\n(get-model)\n" solver;
  let response = read_get_model_response solver in
  let result = Parsing.parse_sygus response ast |> Result.get_ok in
  model_of_sygus_ast result

let rec instantiate_terminals: model_value Utils.StringMap.t -> derivation_tree -> derivation_tree 
= fun model derivation_tree -> 
  let r = instantiate_terminals model in 
  match derivation_tree with 
  | ConcreteIntLeaf (path, _) 
  | SymbolicIntLeaf path -> 
    let path' = String.concat "_" (Utils.init path) |> String.lowercase_ascii in
    print_endline path'; 
    let value = match Utils.StringMap.find path' model with 
    | ConcreteInt int -> int 
    in
    ConcreteIntLeaf (path, value)
  | Node (nt, idx, uid, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, children)

let rec fill_unconstrained_nonterminals: derivation_tree -> derivation_tree 
= fun derivation_tree -> 
  let r = fill_unconstrained_nonterminals in 
  match derivation_tree with 
  | ConcreteIntLeaf _ -> derivation_tree 
  | SymbolicIntLeaf path -> 
    ConcreteIntLeaf (path, random_int_in_range (-100) 100)
  | Node (nt, idx, uid, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, children)

let rec is_complete derivation_tree = match derivation_tree with
| SymbolicIntLeaf _ -> false 
| ConcreteIntLeaf _ -> true 
| Node (_, _, _, children) -> 
  let children = List.map is_complete !children in
  List.length children > 0 && List.fold_left (&&) true children

let rec serialize_derivation_tree: derivation_tree -> string 
= fun derivation_tree -> match derivation_tree with
| SymbolicIntLeaf _ -> Utils.crash "Encountered symbolic leaf during serialization in serialize_derivation_tree"
| ConcreteIntLeaf (_, i) -> string_of_int i 
| Node (_, _, _, children) -> 
  let children = List.map serialize_derivation_tree !children in 
  String.concat "" children

(* TODO: Handle semantic constraints *)
let dpll: A.il_type Utils.StringMap.t -> A.ast -> string
= fun ctx ast -> 
  Random.self_init (); 

  let start_symbol = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _) -> nt
  | ProdRule (nt, _) -> nt
  in 
  (* Set up the key data structures *)
  let derivation_tree = ref (Node (start_symbol, ref None, [start_symbol], ref [])) in 
  let frontier = ref (DTSet.singleton derivation_tree) in 
  let declared_variables = ref Utils.StringSet.empty in 

  let solver = initialize_cvc5 () in

  (* Iteratively expand the frontier and instantiate the derivation tree leaves *)
  while not (DTSet.is_empty !frontier) do 
    let node_to_expand = DTSet.min_elt !frontier in

    (* Expand the chosen frontier node *)
    let _ = match !node_to_expand with 
    | SymbolicIntLeaf _
    | ConcreteIntLeaf _ -> () (* Nothing to expand *)
    | Node (_, _, _, children) when not (List.length !children = 0) -> 
      Utils.crash "Trying to expand a node that already has children in dpll"
    | Node (nt, _idx, path, children) -> 
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> String.equal nt nt2
      ) ast in 
      match grammar_rule with 
      | A.TypeAnnotation (_, Int, []) -> 
        children := [SymbolicIntLeaf (path @ [nt])]
      | A.TypeAnnotation (_, Int, scs) -> 
        List.iter (fun sc -> match sc with 
        | A.SyGuSExpr expr ->
          let path' = String.concat "_" path |> String.lowercase_ascii in
          let path'' = String.concat "_" (Utils.init path) |> String.lowercase_ascii in
          declare_smt_variables declared_variables (Utils.StringMap.singleton path' A.Int) solver;
          assert_smt_constraint path'' solver expr; 
          children := [SymbolicIntLeaf (path @ [nt])];
          let model = get_smt_result ast solver in  
          derivation_tree := instantiate_terminals model !derivation_tree; 
        | A.Dependency _ -> ()
        ) scs
      | A.TypeAnnotation _ -> Utils.crash "Unsupported"
      | A.ProdRule (_, rhss) -> 
        let chosen_rule = List.hd rhss in
        match chosen_rule with 
        | A.StubbedRhs _ -> Utils.crash "Unexpected case in dpll";
        | A.Rhs (ges, scs) -> 
          List.iter (fun sc -> match sc with 
          | A.SyGuSExpr expr ->
            let path' =  String.concat "_" path |> String.lowercase_ascii in
            let expr_variables = A.get_nts_from_expr2 expr in
            let ty_ctx = List.fold_left (fun acc nt -> 
              let ty = Utils.StringMap.find (List.rev nt |> List.hd |> fst) ctx in 
              let str = Format.asprintf "%a" (Lib.pp_print_list Sygus.pp_print_nt_helper "_") nt in
              let str = path' ^ "_" ^ str in 
              Utils.StringMap.add str ty acc
            ) Utils.StringMap.empty expr_variables in
            declare_smt_variables declared_variables ty_ctx solver;
            assert_smt_constraint path' solver expr; 
            let _model = get_smt_result ast solver in
            ()  
            (* don't instantiate yet -- we haven't hit the leaf nodes *)
            (* derivation_tree := instantiate_terminals model !derivation_tree;  *)
          | A.Dependency _ -> ()
          ) scs;
          children := List.map (fun ge -> match ge with 
          | A.Nonterminal nt -> 
            Node (nt, ref None, path @ [nt], ref []) 
          | StubbedNonterminal _ -> Utils.crash "Unexpected case in dpll"
          ) ges;
      in

    (* Update the frontier *)
    frontier := DTSet.remove node_to_expand !frontier;
    frontier := match !node_to_expand with 
    | SymbolicIntLeaf _ 
    | ConcreteIntLeaf _ -> !frontier
    | Node (_, _, _, children) -> 
      List.fold_left (fun acc child ->
        DTSet.add (ref child) acc
      ) !frontier !children;
  done;

  derivation_tree := fill_unconstrained_nonterminals !derivation_tree;
  serialize_derivation_tree !derivation_tree