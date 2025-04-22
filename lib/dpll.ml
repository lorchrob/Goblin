module A = Ast
module SA = SygusAst

(*
A -> B C { B.F < C.J } | D E
B -> F G | H I
C -> J K | L M
_ :: Int
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

Ideas to implement
  * Choosing which production rule option to expand
    * Depth-first, left-first
    * Coin flip. Better (in terms of randomness of generated terms), 
      but then you need to track visited paths.
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

let i = ref 0
let j = ref 0
let uid = ref 0

type model_value = 
| ConcreteInt of int

type derivation_tree = 
| SymbolicIntLeaf of int (* int is uniue ID *)
| ConcreteIntLeaf of int (* int is value of the leaf *)
(* label, index, unique ID, children *)
(* The index is of which production rule option you chose, to enable backtracking *)
| Node of string * int option ref * int * derivation_tree list ref

module DTSet = Set.Make(struct
  type t = derivation_tree ref
  let compare = Stdlib.compare
end)

module CSet = Set.Make(struct
  type t = A.expr 
  let compare = Stdlib.compare
end)

module CSetSet = Set.Make(struct
  type t = CSet.t 
  let compare = CSet.compare
end)

module CSetMap = Map.Make(struct
  type t = CSet.t 
  let compare = CSet.compare
end)

let random_int_in_range: int -> int -> int
= fun min max ->
  Random.self_init (); 
  min + Random.int (max - min + 1) 

let declare_smt_variables _ctx _renaming _filename = ()

let assert_smt_constraint: string -> Ast.expr -> unit 
= fun filename expr ->
  let expr_smtlib_string = Utils.capture_output (Sygus.pp_print_expr Utils.StringMap.empty) expr in 
  let expr_smtlib_string = Format.asprintf "(assert %s)\n(get-model)\n" expr_smtlib_string in
  Utils.write_to_file filename expr_smtlib_string

let rec model_of_sygus_ast: SygusAst.sygus_ast -> model_value Utils.IntMap.t 
= fun sygus_ast -> 
  let r = model_of_sygus_ast in 
  match sygus_ast with 
  | VarLeaf _ 
  | BLLeaf _ 
  | BVLeaf _ -> Utils.crash "Unsupported case in model_of_sygus_ast"
  | Node (constructor, [IntLeaf value]) -> 
    Utils.IntMap.singleton value (ConcreteInt (int_of_string constructor))
  | Node (_, children) -> 
    List.fold_left (fun acc child -> 
      let map = r child in 
      Utils.IntMap.merge Lib.union_keys acc map  
    ) Utils.IntMap.empty children
  | IntLeaf _ -> Utils.crash "Unexpected case in model_of_sygus_ast"
  

let get_smt_result: A.ast -> string -> model_value Utils.IntMap.t 
= fun ast filename -> 
  let cvc5 = Utils.find_command_in_path "cvc5" in
  let (output_filename, output_channel) = Filename.open_temp_file "cvc5_out" ".smt2" in
  close_out output_channel;
  let command = 
    Printf.sprintf "timeout 3 %s --produce-models --dag-thresh=0 %s > %s 2>/dev/null" 
      cvc5 filename output_filename 
  in
  let exit_status = Sys.command command in
  if exit_status <> 0 then
    failwith (Printf.sprintf "CVC5 failed with exit status %d on file %s" exit_status filename);
  let result = Parsing.parse_sygus output_filename ast |> Result.get_ok in
  Sys.remove output_filename;
  model_of_sygus_ast result

let rec instantiate_terminals: model_value Utils.IntMap.t -> derivation_tree -> derivation_tree 
= fun model derivation_tree -> 
  let r = instantiate_terminals model in 
  match derivation_tree with 
  | ConcreteIntLeaf _ -> derivation_tree 
  | SymbolicIntLeaf uid -> 
    let value = match Utils.IntMap.find uid model with 
    | ConcreteInt int -> int 
    in
    ConcreteIntLeaf value
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
| ConcreteIntLeaf i -> string_of_int i 
| Node (_, _, _, children) -> 
  let children = List.map serialize_derivation_tree !children in 
  List.fold_left (^) "" children

(* TODO: Handle semantic constraints *)
let dpll: A.ast -> string
= fun ast -> 
  let start_symbol = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _) -> nt
  | ProdRule (nt, _) -> nt
  in 
  (* Set up the key data structures *)
  let derivation_tree = ref (Node (start_symbol, ref None, !uid, ref [])) in 
  uid := !uid + 1;
  let frontier = ref (DTSet.singleton derivation_tree) in
  let _disjoint_sets = ref CSetSet.empty in 
  let _hanging_nonterminal_map = ref CSetMap.empty in 

  (* Iteratively expand the frontier and instantiate the derivation tree leaves *)
  while not (DTSet.is_empty !frontier) do 
    let node_to_expand = DTSet.min_elt !frontier in

    (* Expand the chosen frontier node *)
    let _ = match !node_to_expand with 
    | SymbolicIntLeaf _
    | ConcreteIntLeaf _ -> () (* Nothing to expand *)
    | Node (_, _, _, children) when not (List.length !children = 0) -> 
      Utils.crash "Trying to expand a node that already has children in dpll"
    | Node (nt, _idx, _, children) -> 
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> String.equal nt nt2
      ) ast in 
      match grammar_rule with 
      | A.TypeAnnotation (_nt, Int, []) -> 
        children := [ConcreteIntLeaf (random_int_in_range (-100) 100)]
      | A.TypeAnnotation (_nt, Int, scs) -> 
        List.iter (fun sc -> match sc with 
          | A.SyGuSExpr expr ->
            let expr, renaming = var_names_to_unique_ids expr in
            declare_smt_variables ctx renaming ("./examples/out/test" ^ string_of_int !j);
            assert_smt_constraint ("./examples/out/test" ^ string_of_int !j) expr; 
            children := [SymbolicIntLeaf (!uid)];
            let model = get_smt_result ast ("./examples/out/test" ^ string_of_int !j) in  
            derivation_tree := instantiate_terminals model !derivation_tree;
            uid := !uid + 1;
            j := !j + 1
          | A.Dependency _ -> ()
        ) scs;
      | A.TypeAnnotation _ -> Utils.crash "Unsupported"
      | A.ProdRule (_nt, rhss) -> 
        let chosen_rule = List.hd rhss in
        children := match chosen_rule with 
        | A.StubbedRhs _ -> Utils.crash "Unexpected case in dpll";
        | A.Rhs (ges, _scs) -> 
          List.map (fun ge -> match ge with 
          | A.Nonterminal nt -> 
            let n = Node (nt, ref None, !uid, ref []) in 
            uid := !uid + 1; 
            n
          | StubbedNonterminal _ -> SymbolicIntLeaf (-1) (* TODO: Think about this case *)
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

  serialize_derivation_tree !derivation_tree