module A = Ast
module SA = SygusAst
module B = Batteries

let (let*) = Res.(>>=)
(*
A -> B C { B.F < C.J } | D E
B -> F G | H I
C -> J K | L M
_ :: Int
*)

(* 
  TODO:
  * When backtracking, make sure the frontier is properly updated
    * Naive solution: recalculate the frontier from scratch at every iteration
  * Handle dependent terms (later in pipeline)
  * Make sure ambiguous references handled properly
  * IDS
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
| ConcreteBool of bool
| ConcreteInt of int
| ConcretePlaceholder of string
| ConcreteString of string
| ConcreteBitVector of int * bool list
| ConcreteBitList of bool list

type derivation_tree = 
| SymbolicLeaf of A.il_type * string list (* path to this node *)
| DependentTermLeaf of string 
| ConcreteBoolLeaf of string list * bool (* path to this node, value of the leaf *)
| ConcreteIntLeaf of string list * int 
| ConcretePlaceholderLeaf of string list * string 
| ConcreteStringLeaf of string list * string
| ConcreteBitVectorLeaf of string list * int * bool list 
| ConcreteBitListLeaf of string list * bool list
(* label, visited set, path to this node, children *)
| Node of string * Utils.IntSet.t ref * string list * derivation_tree list ref

let rec pp_print_derivation_tree ppf derivation_tree = match derivation_tree with 
| SymbolicLeaf _ -> Format.pp_print_string ppf "sym_leaf"
| DependentTermLeaf _ -> Format.pp_print_string ppf "dep_sym_leaf"
| ConcreteBoolLeaf (_, b) -> Format.pp_print_bool ppf b 
| ConcreteIntLeaf (_, int) -> Format.pp_print_int ppf int 
| ConcretePlaceholderLeaf (_, ph) -> Format.fprintf ppf "%S" ph 
| ConcreteStringLeaf (_, str) -> Format.pp_print_string ppf str 
| ConcreteBitVectorLeaf (_, _, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| ConcreteBitListLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "(BitList 0b%a)"
    (Lib.pp_print_list Format.pp_print_int "") bits
| Node (nt, _, _, children) -> 
  Format.fprintf ppf "(%a %a)"
    Format.pp_print_string nt 
    (Lib.pp_print_list pp_print_derivation_tree " ") !children

module DTSet = Set.Make(struct
  type t = derivation_tree ref
  let compare = Stdlib.compare
end)

module ConstraintSet = Set.Make(struct
  type t = A.expr
  let compare = Stdlib.compare
end)

let random_int_in_range: int -> int -> int
= fun min max ->
  min + Random.int (max - min + 1) 

let issue_solver_command: string -> solver_instance -> unit 
= fun cmd_string solver -> 
  if !Flags.debug then Format.pp_print_string Format.std_formatter ("Issuing " ^ cmd_string ^ "\n");
  output_string solver.out_channel cmd_string;
  flush solver.out_channel

let declare_smt_variables: Utils.StringSet.t ref -> A.il_type Utils.StringMap.t -> solver_instance -> unit 
= fun declared_variables ctx solver -> 
  Utils.StringMap.iter (fun var ty -> 
    if Utils.StringSet.mem var !declared_variables then 
      () 
    else 
      let declaration_string = Format.asprintf "(declare-fun %s () %a)\n" var Sygus.pp_print_ty ty in
      declared_variables := Utils.StringSet.add var !declared_variables;
      issue_solver_command declaration_string solver
  ) ctx 

let read_check_sat_response solver = 
    input_line solver.in_channel

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
  let set_logic_command = Format.asprintf "(set-logic QF_BVSLIA)\n" in
  let solver = { in_channel = in_chan; out_channel = out_chan; err_channel = err_chan } in
  issue_solver_command set_logic_command solver;
  solver

let assert_smt_constraint: solver_instance -> Ast.expr -> unit 
= fun solver expr ->
  let assert_cmd = 
    Format.asprintf "(assert %a)\n" (Sygus.pp_print_expr Utils.StringMap.empty) expr 
  in
  issue_solver_command assert_cmd solver; 
  ()

(* State expression nonterminals in terms of absolute paths from 
   the root of the derivation tree *)
let rec universalize_expr is_type_annotation prefix expr = 
  let r = universalize_expr is_type_annotation prefix in
  match expr with
  | A.NTExpr (nts1, nts2) -> 
    (* In the derivation tree structure, type annotation NTs have a duplicate at the end of the path. 
       Remove it. *)
    let prefix = if is_type_annotation then Utils.init prefix else prefix in
    A.NTExpr (nts1, List.map (fun id -> id, None) prefix @ nts2)
  | BVCast (len, expr) -> BVCast (len, r expr)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2) 
  | UnOp (op, expr) -> UnOp (op, r expr) 
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2) 
  | StrLength expr -> StrLength (r expr)
  | Length expr -> Length (r expr) 
  | Match _ -> Utils.crash "Unexpected case in universalize_expr"
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

let new_decision_level: solver_instance -> unit 
= fun solver ->
  let push_cmd = Format.asprintf "(push %d)" !assertion_level in
  assertion_level := !assertion_level + 1;
  issue_solver_command push_cmd solver; 
  ()

let backtrack_decision_level: solver_instance -> unit 
= fun solver ->
  if !assertion_level = 0 then (print_endline "infeasible"; exit 0);
  let pop_cmd = Format.asprintf "(pop 1)" in
  assertion_level := !assertion_level - 1;
  issue_solver_command pop_cmd solver; 
  ()

let rec model_of_sygus_ast: SygusAst.sygus_ast -> (model_value Utils.StringMap.t, unit) result
= fun sygus_ast -> 
  match sygus_ast with 
  | VarLeaf var when var = "infeasible" -> Error ()
  | Node (constructor, [IntLeaf value]) -> 
    Ok (Utils.StringMap.singleton constructor (ConcreteInt value))
  | Node (constructor, [StrLeaf value]) -> 
    Ok (Utils.StringMap.singleton constructor (ConcreteString value))
  | Node (constructor, [BoolLeaf value]) -> 
    Ok (Utils.StringMap.singleton constructor (ConcreteBool value))
  | Node (constructor, [BLLeaf value]) -> 
    Ok (Utils.StringMap.singleton constructor (ConcreteBitList value))
    | Node (constructor, [BVLeaf (len, value)]) -> 
    Ok (Utils.StringMap.singleton constructor (ConcreteBitVector (len, value)))
  | Node (_, children) -> 
    (Res.seq_chain (fun acc child -> 
      let* map = model_of_sygus_ast child in 
      Ok (Utils.StringMap.merge Lib.union_keys acc map)  
    ) Utils.StringMap.empty children)
  | VarLeaf _ | BLLeaf _ | BVLeaf _ 
  | BoolLeaf _ | StrLeaf _ | IntLeaf _ -> Utils.crash "Unexpected case in model_of_sygus_ast"

let get_smt_result: A.ast -> solver_instance -> (model_value Utils.StringMap.t, unit) result
= fun ast solver -> 
  issue_solver_command "(check-sat)\n" solver;
  let response = read_check_sat_response solver in
  if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
  if response = "sat" then (
    issue_solver_command "(get-model)\n" solver;
    let response = read_get_model_response solver in
    if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
    let result = Parsing.parse_sygus response ast |> Result.get_ok in
    model_of_sygus_ast result
  ) 
  else
    let result = Parsing.parse_sygus "unsat" ast |> Result.get_ok in
    model_of_sygus_ast result

let ty_of_concrete_leaf leaf = match leaf with 
| ConcreteBitListLeaf _ -> A.BitList 
| ConcreteIntLeaf _ -> Int 
| ConcreteStringLeaf _ -> String 
| ConcretePlaceholderLeaf _ -> Placeholder 
| ConcreteBitVectorLeaf (_, n, _) -> BitVector n
| ConcreteBoolLeaf _ -> Bool 
| _ -> Utils.crash "Unexpected case in ty_of_concrete_leaf"

let rec instantiate_terminals: model_value Utils.StringMap.t -> derivation_tree -> derivation_tree 
= fun model derivation_tree -> 
  let r = instantiate_terminals model in 
  match derivation_tree with 
  | ConcreteIntLeaf (path, _) | ConcreteBoolLeaf (path, _) | ConcreteBitListLeaf (path, _)
  | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) | ConcreteStringLeaf (path, _) -> 
    let path' = String.concat "_" (Utils.init path) |> String.lowercase_ascii in
    (match Utils.StringMap.find_opt path' model with 
    | Some (ConcreteInt int) -> ConcreteIntLeaf (path, int)
    | Some (ConcreteBool bool) -> ConcreteBoolLeaf (path, bool)
    | Some (ConcreteBitList bitlist) -> ConcreteBitListLeaf (path, bitlist)
    | Some (ConcreteBitVector (len, bits)) -> ConcreteBitVectorLeaf (path, len, bits)
    | Some (ConcreteString str) -> ConcreteStringLeaf (path, str)
    | Some (ConcretePlaceholder ph) -> ConcretePlaceholderLeaf (path, ph)
    | None -> SymbolicLeaf (ty_of_concrete_leaf derivation_tree, path) (* Model contain unconstrained variables not present in this derivation tree *))
  | SymbolicLeaf (ty, path) -> 
    let path' = String.concat "_" (Utils.init path) |> String.lowercase_ascii in
    (match Utils.StringMap.find_opt path' model with 
    | Some (ConcreteInt int) -> ConcreteIntLeaf (path, int)
    | Some (ConcreteBool bool) -> ConcreteBoolLeaf (path, bool)
    | Some (ConcreteBitList bitlist) -> ConcreteBitListLeaf (path, bitlist)
    | Some (ConcreteBitVector (len, bits)) -> ConcreteBitVectorLeaf (path, len, bits)
    | Some (ConcreteString str) -> ConcreteStringLeaf (path, str)
    | Some (ConcretePlaceholder ph) -> ConcretePlaceholderLeaf (path, ph)
    | None -> SymbolicLeaf (ty, path) (* Model contain unconstrained variables not present in this derivation tree *))
  | DependentTermLeaf _ -> derivation_tree
  | Node (nt, idx, uid, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, children)
  
let rec fill_unconstrained_nonterminals: derivation_tree -> derivation_tree 
= fun derivation_tree -> 
  let r = fill_unconstrained_nonterminals in 
  match derivation_tree with 
  | ConcreteIntLeaf _ | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ 
  | ConcreteBoolLeaf _ | ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ -> derivation_tree 
  | SymbolicLeaf (Int, path) -> 
    ConcreteIntLeaf (path, random_int_in_range (-100) 100)
  | SymbolicLeaf (Bool, path) -> 
    ConcreteBoolLeaf (path, Random.bool ())
  | SymbolicLeaf (BitList, path) -> 
    ConcreteBitListLeaf (path, Utils.random_bools (random_int_in_range 0 25))
  | SymbolicLeaf (BitVector n, path) -> 
    ConcreteBitVectorLeaf (path, n, Utils.random_bools n)
  | SymbolicLeaf (Placeholder, path) -> 
    ConcretePlaceholderLeaf (path, "generated_placeholder")
  | SymbolicLeaf (String, path) -> 
    ConcreteStringLeaf (path, Utils.random_string (random_int_in_range 0 25))
  | SymbolicLeaf (ADT _, _) -> Utils.crash "Unexpected case in fill_unconstrained_nonterminals"
  | DependentTermLeaf _ -> derivation_tree
  | Node (nt, idx, uid, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, children)

let rec is_complete derivation_tree = match derivation_tree with
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ 
| ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ -> true
| Node (_, _, _, children) -> 
  let children = List.map is_complete !children in
  List.length children > 0 && List.fold_left (&&) true children 

let rec sygus_ast_of_derivation_tree: derivation_tree -> SA.sygus_ast 
= fun derivation_tree -> match derivation_tree with
| DependentTermLeaf nt -> VarLeaf (String.lowercase_ascii (nt ^ "_con")) (* Match sygus encoding format *)
| SymbolicLeaf (_, path) -> VarLeaf (String.concat "" path)
| ConcreteIntLeaf (_, i) -> IntLeaf i
| ConcreteBitListLeaf (_, bits) -> BLLeaf bits 
| ConcreteBitVectorLeaf (_, len, bits) -> BVLeaf (len, bits)
| ConcretePlaceholderLeaf (_, ph) -> VarLeaf ph 
| ConcreteStringLeaf (_, str) -> StrLeaf str
| ConcreteBoolLeaf (_, b) -> BoolLeaf b
| Node (nt, _, _, children) -> 
  let children = List.map sygus_ast_of_derivation_tree !children in 
  Node (nt, children)

(* Naive computation of frontier *)
let rec compute_new_frontier derivation_tree = match derivation_tree with 
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ 
| ConcreteBoolLeaf _ | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ -> DTSet.empty
| Node (_, _, _, children) as node -> 
  if !children = [] then DTSet.singleton (ref node) else
  List.fold_left (fun acc child -> 
    DTSet.union acc (compute_new_frontier child)
  ) DTSet.empty !children 


(* Determine whether an nt, in dot notation, will __necessarily__ be reached 
   in a given derivation_tree *)
let rec nt_will_be_reached derivation_tree ast nt = 
  (* at each step, check if the rest of nt is reachable from head *)
  let rec nt_will_be_reached_ast nt head = match nt with 
  | [] -> true 
  | (new_head, _) :: nts -> 
    let rule = List.find (fun element -> match element with
    | A.TypeAnnotation (nt2, _, _) 
    | A.ProdRule (nt2, _) -> nt2 = head
    ) ast in 
    match rule with 
    | ProdRule (_, rhss) -> 
      (* There are multiple options. Conservatively say the nt may not be reached. 
         TODO: This is a safe approximation. A more exact analysis would see if the nt is reachable in all cases. *)
      if List.length rhss <> 1 then false
      else 
        nt_will_be_reached_ast nts new_head
    | _ -> true
  in
  match nt with 
  | [] -> true 
  | (str, _) :: nts -> match derivation_tree with 
    | Node (head, _, _, children) -> 
      if !children = [] then nt_will_be_reached_ast nt head
      else (
        match List.find_opt (fun child -> match child with 
        | Node (nt2, _, _, _) -> str = nt2
        | SymbolicLeaf (_, path) -> str = Utils.last path
        | ConcreteIntLeaf (path, _) | ConcreteBoolLeaf (path, _) | ConcreteBitListLeaf (path, _) 
        | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) | ConcreteStringLeaf (path, _) -> str = Utils.last path
        | DependentTermLeaf nt2 -> str = nt2
        ) !children with 
        | Some child -> nt_will_be_reached child ast nts
        | None -> 
          if !Flags.debug then Format.fprintf Format.std_formatter "Could not find child %s from node %s\n"
            str head;
          false)
    | _ -> true

(* Determine if a constraint necessarily applies to a given derivation tree. 
   It may not apply if the nonterminals referenced by the constraint are avoidable 
   by selecting other production rule options in the AST. *)
let constraint_is_applicable expr derivation_tree ast = 
  let nts = A.get_nts_from_expr2 expr in
  List.fold_left (fun acc nt -> 
    acc && nt_will_be_reached derivation_tree ast (List.tl nt)
  ) true nts

let assert_and_remove_applicable_constraints constraint_set derivation_tree ast solver =
  let constraints_to_remove = 
    ConstraintSet.fold (fun expr acc -> 
      if constraint_is_applicable expr derivation_tree ast then (
        (* declare_smt_variables declared_variables (Utils.StringMap.singleton path' A.Int) solver; *)
        if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is applicable in derivation tree %a\n"
          A.pp_print_expr expr 
          pp_print_derivation_tree derivation_tree;
        assert_smt_constraint solver expr;
        ConstraintSet.add expr acc
      ) else (
        (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is not applicable in derivation tree %a\n"
          A.pp_print_expr expr 
          pp_print_derivation_tree derivation_tree);
        acc
      )
    )  !constraint_set ConstraintSet.empty 
  in
  constraint_set := ConstraintSet.diff !constraint_set constraints_to_remove;
  !constraint_set

let backtrack_derivation_tree decision_stack = 
  if B.Stack.is_empty decision_stack then (print_endline "infeasible"; exit 0);
  match !(B.Stack.pop decision_stack) with 
  | Node (_, _, _, children) -> 
    children := []
  | _ -> Utils.crash "Unexpected case in backtrack_derivation_tree"

let dpll: A.il_type Utils.StringMap.t -> A.ast -> SA.sygus_ast
= fun ctx ast -> 
  Random.self_init (); 

  let start_symbol = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _) -> nt
  | ProdRule (nt, _) -> nt
  in 

  (*** Set up the key data structures ***)
  (* Incremental construction of output term so far *)
  let derivation_tree = ref (Node (start_symbol, ref Utils.IntSet.empty, [start_symbol], ref [])) in 
  (* Tree nodes left to explore *)
  let frontier = ref (DTSet.singleton derivation_tree) in 
  (* Track declared (SMT-level) variables to avoid redeclaration *)
  let declared_variables = ref Utils.StringSet.empty in 
  (* Keep around constraints we may not need to assert *)
  let constraints_to_assert = ref ConstraintSet.empty in 
  (* Set of paths through the tree to help determine when to push constraints from constraints_to_assert *)
  (* let provenance_list = _ in *) (* Challenge: backtracking affects provenance list *)
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  let decision_stack : derivation_tree ref Stack.t = B.Stack.create () in 
  (* Solver object *)
  let solver = initialize_cvc5 () in

  (* Iteratively expand the frontier and instantiate the derivation tree leaves *)
  while not (is_complete !derivation_tree) do 
    (* It is unclear how to efficiently update the frontier with backtracking in general. 
    IT seems straightforward with DPLL-style backtracking, but not backjumping. So for now, 
    we compute it naively. *)
    frontier := compute_new_frontier !derivation_tree;

    if !Flags.debug then Format.fprintf Format.std_formatter "------------------------\n";
    if !Flags.debug then Format.fprintf Format.std_formatter "Constraints to assert: %a\n"
      (Lib.pp_print_list A.pp_print_expr " ") (ConstraintSet.elements !constraints_to_assert);
    if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
      pp_print_derivation_tree !derivation_tree;
    if !Flags.debug then Format.fprintf Format.std_formatter "Frontier: %a\n"
      (Lib.pp_print_list pp_print_derivation_tree "; ") (DTSet.elements !frontier |> List.map (fun p -> !p));

    let node_to_expand = DTSet.choose !frontier in

    (* TODO: Make sure there are no constraints in constraints_to_assert hanging around that still 
       need to be asserted. When one of these constraints is asserted after some delay, have 
       to figure out some backtracking logic. 
       Need to mix in some pops. 
       
       Maybe try to assert all constraints_to_assert after any new decision. 
       In principle, only new decisions should "unlock" new constraints.

       Backtracking:
       After every __decision__ (real decision), push constraints at new (incremented) decision level. 
       
       If unsat, pop constraints (decrementing decision level), and backtrack to the most recent decision to flip on the derivation tree. 
       
       If unsat at decision level 0, then globally unsat. *)

    (* Expand the chosen frontier node *)
    match !node_to_expand with 
    | SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ 
    | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
    | ConcretePlaceholderLeaf _ | ConcreteStringLeaf _  -> () (* nothing else to do *)
    | Node (nt, _, _, children) when not (List.length !children = 0) -> 
      Utils.crash ("Trying to expand node " ^ nt ^ " that already has children in dpll")
    | Node (nt, visited, path, children) as node -> 
      let path' = String.concat "_" path |> String.lowercase_ascii in
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> String.equal nt nt2
      ) ast in 
      match grammar_rule with 
      | A.TypeAnnotation (_, ty, []) -> 
        visited := Utils.IntSet.add 0 !visited;
        children := [SymbolicLeaf (ty, path @ [nt])];
      | A.TypeAnnotation (_, ty, scs) -> 
        visited := Utils.IntSet.add 0 !visited;
        (* Assert semantic constraints for type annotations *)
        List.iter (fun sc -> match sc with 
        | A.SyGuSExpr expr ->
          declare_smt_variables declared_variables (Utils.StringMap.singleton path' ty) solver; 
          constraints_to_assert := ConstraintSet.add (universalize_expr true path expr) !constraints_to_assert;
          constraints_to_assert := assert_and_remove_applicable_constraints constraints_to_assert !derivation_tree ast solver;
          let model = get_smt_result ast solver in  
          (match model with 
          | Ok model -> 
            children := [SymbolicLeaf (ty, path @ [nt])]; 
            derivation_tree := instantiate_terminals model !derivation_tree; 
          | Error () -> 
            backtrack_decision_level solver;
            backtrack_derivation_tree decision_stack)
        | A.Dependency _ -> ()
        ) scs;
      | A.ProdRule (_, rhss) -> 
        (* At every real choice, increase the decision level *)
        if (List.length rhss - Utils.IntSet.cardinal !visited) > 1 then (
          new_decision_level solver;
          B.Stack.push (ref node) decision_stack;
        );
        let idx, chosen_rule = Utils.fresh_random_element !visited rhss in
        if !Flags.debug then Format.fprintf Format.std_formatter "Chose rule index %d, rule %a\n" 
          idx
          A.pp_print_prod_rule_rhs chosen_rule;
        visited := Utils.IntSet.add idx !visited;
        match chosen_rule with 
        | A.StubbedRhs str -> 
          children := [DependentTermLeaf str]
        | A.Rhs (ges, scs) -> 
          children := List.map (fun ge -> match ge with 
          | A.Nonterminal nt -> 
            Node (nt, ref Utils.IntSet.empty, path @ [nt], ref [])  
          | StubbedNonterminal (_, nt) -> DependentTermLeaf nt
          ) ges;
          List.iter (fun sc -> match sc with 
          | A.SyGuSExpr expr ->
            (* Assert semantic constraints for production rules *)
            let expr_variables = A.get_nts_from_expr2 expr in
            let ty_ctx = List.fold_left (fun acc nt -> 
              let ty = Utils.StringMap.find (List.rev nt |> List.hd |> fst) ctx in 
              let str = Format.asprintf "%a" (Lib.pp_print_list Sygus.pp_print_nt_helper "_") nt in
              let str = path' ^ "_" ^ str in 
              Utils.StringMap.add str ty acc
            ) Utils.StringMap.empty expr_variables in
            declare_smt_variables declared_variables ty_ctx solver;
            constraints_to_assert := ConstraintSet.add (universalize_expr false path expr) !constraints_to_assert;
            (* don't instantiate yet -- we haven't hit the leaf nodes *)
            (* derivation_tree := instantiate_terminals model !derivation_tree;  *)
          | A.Dependency _ -> ()
          ) scs;

          (* Assert the constraints from this choice (and also try to assert constraints hanging around from earlier on,
             but maybe weren't definitely applicable until this decision) *)
          constraints_to_assert := assert_and_remove_applicable_constraints constraints_to_assert !derivation_tree ast solver;
          let model = get_smt_result ast solver in
          (match model with 
          | Ok _ -> (* sat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter "it was SAT, waiting to expand before instantiating in derivation tree\n"; 
          | Error () -> (* unsat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
            backtrack_decision_level solver;
            backtrack_derivation_tree decision_stack); 

    
    (* Update the frontier *)
    (* Only expand the frontier if the new constraints were SAT; otherwise, just go to next iteration (we updated the visited set) *)
    (* if expand then (
      frontier := DTSet.remove node_to_expand !frontier;
      frontier := match !node_to_expand with 
      | SymbolicIntLeaf _ 
      | ConcreteIntLeaf _ 
      | DependentTermLeaf _ -> !frontier
      | Node (_, _, _, children) -> 
        List.fold_left (fun acc child ->
          DTSet.add (ref child) acc
        ) !frontier !children;
    ); *)
  done;

  if !Flags.debug then Format.fprintf Format.std_formatter "Constraints to assert: %a\n"
    (Lib.pp_print_list A.pp_print_expr " ") (ConstraintSet.elements !constraints_to_assert);
  if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
    pp_print_derivation_tree !derivation_tree;

  (* Handle any remaining constraints from last iteration of the loop *)
  let model = get_smt_result ast solver in
  (match model with 
  | Ok model -> (* sat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was SAT, instantiating in derivation tree\n"; 
    derivation_tree := instantiate_terminals model !derivation_tree; 
  | Error () -> (* unsat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
    backtrack_decision_level solver;
    backtrack_derivation_tree decision_stack); 

  derivation_tree := fill_unconstrained_nonterminals !derivation_tree;
  (* Convert to sygus AST for later processing in the pipeline *)
  sygus_ast_of_derivation_tree !derivation_tree