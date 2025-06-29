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
| ConcreteStringSet of Utils.StringSet.t

let rec pp_print_ss ppf = function 
  | [] -> Format.pp_print_string ppf "(as set.empty (Set String))"
  | [s] -> 
    Format.fprintf ppf "(set.singleton %S)"
      s
  | s :: tl -> 
    Format.fprintf ppf "(set.union (set.singleton %S) %a)" 
      s pp_print_ss tl
      
      

let pp_print_model_value ppf = function
| ConcreteBool b -> Format.pp_print_bool ppf b
| ConcreteInt i -> Format.pp_print_int ppf i 
| ConcretePlaceholder str -> Format.pp_print_string ppf str
| ConcreteString str -> Format.fprintf ppf "%S" str 
| ConcreteStringSet ss -> pp_print_ss ppf (Utils.StringSet.to_list ss)
(* TODO: fill in with actual details *)
| ConcreteBitVector _ -> Format.pp_print_string ppf "bitvector"
| ConcreteBitList _ -> Format.pp_print_string ppf "bitlist"

type concrete_set = 
| ConcreteStringSetLeaf of Utils.StringSet.t

type derivation_tree = 
| SymbolicLeaf of A.il_type * (string * int option) list (* path to this node *)
| DependentTermLeaf of string 
| ConcreteBoolLeaf of (string * int option) list * bool (* path to this node, value of the leaf *)
| ConcreteIntLeaf of (string * int option) list * int 
| ConcretePlaceholderLeaf of (string * int option) list * string 
| ConcreteStringLeaf of (string * int option) list * string
| ConcreteSetLeaf of (string * int option) list * concrete_set
| ConcreteBitVectorLeaf of (string * int option) list * int * bool list 
| ConcreteBitListLeaf of (string * int option) list * bool list
(* label, visited set, path to this node, depth, children *)
| Node of (string * int option) * Utils.IntSet.t ref * (string * int option) list * int * derivation_tree list ref

let rec pp_print_derivation_tree ppf derivation_tree = match derivation_tree with 
| SymbolicLeaf _ -> Format.pp_print_string ppf "sym_leaf"
| DependentTermLeaf _ -> Format.pp_print_string ppf "dep_sym_leaf"
| ConcreteBoolLeaf (_, b) -> Format.pp_print_bool ppf b 
| ConcreteIntLeaf (_, int) -> Format.pp_print_int ppf int 
| ConcretePlaceholderLeaf (_, ph) -> Format.fprintf ppf "%S" ph 
| ConcreteStringLeaf (_, str) -> Format.pp_print_string ppf str 
| ConcreteSetLeaf _ -> Format.pp_print_string ppf "concrete_string_set"
| ConcreteBitVectorLeaf (_, _, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| ConcreteBitListLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "(BitList 0b%a)"
    (Lib.pp_print_list Format.pp_print_int "") bits
| Node ((nt, Some idx), _, _, _, children) -> 
  Format.fprintf ppf "(%a%d %a)"
    Format.pp_print_string nt 
    idx
    (Lib.pp_print_list pp_print_derivation_tree " ") !children
| Node ((nt, None), _, _, _, children) -> 
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
    Printf.sprintf "%s --produce-models --global-declarations --dag-thresh=0 --lang=smtlib2 --incremental" 
      cvc5 
  in
  let (in_chan, out_chan, err_chan) = Unix.open_process_full cmd (Unix.environment ()) in
  let set_logic_command = Format.asprintf "(set-logic QF_BVSLIAFS)\n" in
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
let rec universalize_expr: bool -> (string * int option) list -> Ast.expr -> Ast.expr
= fun is_type_annotation prefix expr ->
  let r = universalize_expr is_type_annotation prefix in
  match expr with
  | A.NTExpr (nts1, nts2) -> 
    (* In the derivation tree structure, type annotation NTs have a duplicate at the end of the path. 
       Remove it. *)
    let prefix = if is_type_annotation then Utils.init prefix else prefix in
    A.NTExpr (nts1, prefix @ nts2)
  | BVCast (len, expr) -> BVCast (len, r expr)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2) 
  | UnOp (op, expr) -> UnOp (op, r expr) 
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2) 
  | StrLength expr -> StrLength (r expr)
  | Length expr -> Length (r expr) 
  | Singleton expr -> Singleton (r expr)
  | Match _ -> Utils.crash "Unexpected case in universalize_expr"
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ 
  | EmptySet _ -> expr

let new_decision_level: solver_instance -> int ref -> unit 
= fun solver assertion_level ->
  let push_cmd = Format.asprintf "(push 1)" in
  assertion_level := !assertion_level + 1;
  issue_solver_command push_cmd solver; 
  ()

let backtrack_decision_level: solver_instance -> int ref -> unit
= fun solver assertion_level ->
  if !assertion_level = 1 then ( (* restarting *)
    Utils.debug_print Format.pp_print_string Format.std_formatter "Restarting...\n";
    issue_solver_command "(pop 1)" solver; 
    issue_solver_command "(push 1)" solver;
  ) else ( 
    assertion_level := !assertion_level - 1;
    issue_solver_command "(pop 1)" solver; 
    ()
  )

let string_of_constructor (str, idx) = match idx with 
| None -> str 
| Some idx -> str ^ (string_of_int idx)

let rec model_of_sygus_ast: SygusAst.sygus_ast -> (model_value Utils.StringMap.t, unit) result
= fun sygus_ast -> 
  match sygus_ast with 
  | VarLeaf var when var = "infeasible" -> Error ()
  | Node (constructor, [SetLeaf (StringSet value)]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteStringSet value))
  | Node (constructor, [IntLeaf value]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteInt value))
  | Node (constructor, [StrLeaf value]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteString value))
  | Node (constructor, [BoolLeaf value]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteBool value))
  | Node (constructor, [BLLeaf value]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteBitList value))
    | Node (constructor, [BVLeaf (len, value)]) -> 
    Ok (Utils.StringMap.singleton (string_of_constructor constructor) (ConcreteBitVector (len, value)))
  | Node (_, children) -> 
    (Res.seq_chain (fun acc child -> 
      let* map = model_of_sygus_ast child in 
      Ok (Utils.StringMap.merge Lib.union_keys acc map)  
    ) Utils.StringMap.empty children)
  | VarLeaf _ | BLLeaf _ | BVLeaf _ 
  | BoolLeaf _ | StrLeaf _ | IntLeaf _ | SetLeaf _ -> Utils.crash "Unexpected case in model_of_sygus_ast"

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

let string_of_path path = 
  let path = List.map (fun (nt, idx) -> match idx with 
  | None -> nt
  | Some idx -> Format.asprintf "%s%d" nt idx
  ) path in 
  String.concat "_" path

let rec instantiate_terminals: model_value Utils.StringMap.t -> derivation_tree -> derivation_tree 
= fun model derivation_tree -> 
  let r = instantiate_terminals model in 
  match derivation_tree with 
  | ConcreteIntLeaf (path, _) | ConcreteBoolLeaf (path, _) | ConcreteBitListLeaf (path, _)
  | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) | ConcreteStringLeaf (path, _) | ConcreteSetLeaf (path, _) -> 
    let path' = string_of_path (Utils.init path) |> String.lowercase_ascii in
    (match Utils.StringMap.find_opt path' model with 
    | Some (ConcreteInt int) -> ConcreteIntLeaf (path, int)
    | Some (ConcreteBool bool) -> ConcreteBoolLeaf (path, bool)
    | Some (ConcreteBitList bitlist) -> ConcreteBitListLeaf (path, bitlist)
    | Some (ConcreteBitVector (len, bits)) -> ConcreteBitVectorLeaf (path, len, bits)
    | Some (ConcreteString str) -> ConcreteStringLeaf (path, str)
    | Some (ConcreteStringSet s) -> ConcreteSetLeaf (path, ConcreteStringSetLeaf s)
    | Some (ConcretePlaceholder ph) -> ConcretePlaceholderLeaf (path, ph)
    | None -> SymbolicLeaf (ty_of_concrete_leaf derivation_tree, path) (* Model contain unconstrained variables not present in this derivation tree *))
  | SymbolicLeaf (ty, path) -> 
    let path' = string_of_path (Utils.init path) |> String.lowercase_ascii in
    (match Utils.StringMap.find_opt path' model with 
    | Some (ConcreteInt int) -> ConcreteIntLeaf (path, int)
    | Some (ConcreteBool bool) -> ConcreteBoolLeaf (path, bool)
    | Some (ConcreteBitList bitlist) -> ConcreteBitListLeaf (path, bitlist)
    | Some (ConcreteBitVector (len, bits)) -> ConcreteBitVectorLeaf (path, len, bits)
    | Some (ConcreteString str) -> ConcreteStringLeaf (path, str)
    | Some (ConcretePlaceholder ph) -> ConcretePlaceholderLeaf (path, ph)
    | Some (ConcreteStringSet s) -> ConcreteSetLeaf (path, ConcreteStringSetLeaf s)
    | None -> SymbolicLeaf (ty, path) (* Model contain unconstrained variables not present in this derivation tree *))
  | DependentTermLeaf _ -> derivation_tree
  | Node (nt, idx, uid, depth, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, depth, children)
  
let rec fill_unconstrained_nonterminals: derivation_tree -> derivation_tree 
= fun derivation_tree -> 
  let r = fill_unconstrained_nonterminals in 
  match derivation_tree with 
  | ConcreteIntLeaf _ | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ 
  | ConcreteBoolLeaf _ | ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ 
  | ConcreteSetLeaf _ -> derivation_tree 
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
  | SymbolicLeaf (Set ty, path) ->
    let set = match ty with 
    | String -> Utils.StringSet.empty 
    | _ -> Utils.crash "TODO: Support more set types in DPLL module" in
    ConcreteSetLeaf (path, (ConcreteStringSetLeaf set)) 
  | SymbolicLeaf (ADT _, _) -> Utils.crash "Unexpected case in fill_unconstrained_nonterminals"
  | DependentTermLeaf _ -> derivation_tree
  | Node (nt, idx, uid, depth, children) -> 
    children := List.map r !children;
    Node (nt, idx, uid, depth, children)

let rec is_complete derivation_tree = match derivation_tree with
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ 
| ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ | ConcreteSetLeaf _ -> true
| Node (_, _, _, _, children) -> 
  let children = List.map is_complete !children in
  List.length children > 0 && List.fold_left (&&) true children 

let rec sygus_ast_of_derivation_tree: derivation_tree -> SA.sygus_ast 
= fun derivation_tree -> match derivation_tree with
| DependentTermLeaf nt -> VarLeaf (String.lowercase_ascii (nt ^ "_con")) (* Match sygus encoding format *)
| SymbolicLeaf (_, path) -> VarLeaf (String.concat "" (List.map fst path))
| ConcreteIntLeaf (_, i) -> IntLeaf i
| ConcreteBitListLeaf (_, bits) -> BLLeaf bits 
| ConcreteBitVectorLeaf (_, len, bits) -> BVLeaf (len, bits)
| ConcretePlaceholderLeaf (_, ph) -> VarLeaf ph 
| ConcreteStringLeaf (_, str) -> StrLeaf str
| ConcreteSetLeaf (_, (ConcreteStringSetLeaf set)) -> SetLeaf (StringSet set)
| ConcreteBoolLeaf (_, b) -> BoolLeaf b
| Node (nt, _, _, _, children) -> 
  let children = List.map sygus_ast_of_derivation_tree !children in 
  Node (nt, children)

(* Naive computation of frontier *)
let rec compute_new_frontier derivation_tree = match derivation_tree with 
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ | ConcreteSetLeaf _  
| ConcreteBoolLeaf _ | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ -> DTSet.empty
| Node (_, _, _, _, children) as node -> 
  if !children = [] then DTSet.singleton (ref node) else
  List.fold_left (fun acc child -> 
    DTSet.union acc (compute_new_frontier child)
  ) DTSet.empty !children 


(* Determine whether an nt, in dot notation, will __necessarily__ be reached 
   in a given derivation_tree *)
let rec nt_will_be_reached derivation_tree ast nt = 
  (* at each step, check if the rest of nt is reachable from head *)
  let rec nt_will_be_reached_ast: (string * int option) list -> (string * int option) -> bool 
  = fun nt head -> match nt with 
  | [] -> true 
  | new_head :: nts -> 
    let rule = List.find (fun element -> match element with
    | A.TypeAnnotation (nt2, _, _) 
    | A.ProdRule (nt2, _) -> nt2 = (fst head)
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
  | str :: nts -> match derivation_tree with 
    | Node (head, _, _, _, children) -> 
      if !children = [] then nt_will_be_reached_ast nt head
      else (
        match List.find_opt (fun child -> match child with 
        | Node (nt2, _, _, _, _) -> str = nt2
        | SymbolicLeaf (_, path) -> str = (Utils.last path)
        | ConcreteIntLeaf (path, _) | ConcreteBoolLeaf (path, _) | ConcreteBitListLeaf (path, _) 
        | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) | ConcreteStringLeaf (path, _) | ConcreteSetLeaf (path, _) -> str = (Utils.last path)
        | DependentTermLeaf nt2 -> (fst str) = nt2
        ) !children with 
        | Some child -> nt_will_be_reached child ast nts
        | None -> 
          if !Flags.debug then Format.fprintf Format.std_formatter "Could not find child %s from node %s\n"
            (fst str) (fst head);
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

let initialize_globals derivation_tree start_symbol frontier constraints_to_assert decision_stack _declared_variables backtrack_depth = 
  (* Incremental construction of output term so far *)
  derivation_tree := (Node ((start_symbol, None), ref Utils.IntSet.empty, [start_symbol, None], 0, ref []));
  (* Tree nodes left to explore *)
  frontier := (DTSet.singleton derivation_tree); 
  (* Keep around constraints we may not need to assert *)
  constraints_to_assert := ConstraintSet.empty; 
  (* Set of paths through the tree to help determine when to push constraints from constraints_to_assert *)
  (* let provenance_list = _ in *) (* Challenge: backtracking affects provenance list *)
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  decision_stack := B.Stack.create ();
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  backtrack_depth := false; 
  ()

let backtrack_derivation_tree decision_stack start_symbol depth_limit derivation_tree frontier constraints_to_assert declared_variables backtrack_depth = 
  if B.Stack.is_empty !decision_stack then (
    if not !backtrack_depth then raise (Failure "infeasible");
    depth_limit := !depth_limit + 1;
    initialize_globals derivation_tree start_symbol frontier constraints_to_assert decision_stack declared_variables backtrack_depth; 
  ) else (
    match !(B.Stack.pop !decision_stack) with 
    | Node (_, _, _, _, children) -> 
      children := []
    | _ -> Utils.crash "Unexpected case in backtrack_derivation_tree"
  )

let pp_print_model_pair ppf (k, v) = 
  Format.fprintf ppf "(= %s %a)" 
    k 
    pp_print_model_value v 

let push_blocking_clause model declared_variables solver = 
  let ctx_of_model model = Utils.StringMap.map (function 
  | ConcreteBool _ -> A.Bool 
  | ConcreteInt _ -> A.Int 
  | ConcreteString _ -> A.String
  | ConcreteBitList _ -> A.BitList 
  | ConcreteStringSet _ -> A.Set(String)
  | ConcreteBitVector (w, _) -> A.BitVector w
  | ConcretePlaceholder _ -> A.Placeholder 
  ) model in 
  let ctx = ctx_of_model model in 
  declare_smt_variables declared_variables ctx solver; 
  let blocking_clause_str = Format.asprintf "(assert (not (and %a)))" 
    (Lib.pp_print_list pp_print_model_pair " ") (Utils.StringMap.bindings model) in
  issue_solver_command blocking_clause_str solver  



let dpll: A.il_type Utils.StringMap.t -> A.ast -> SA.sygus_ast
= fun ctx ast -> 
  Random.self_init (); 
  let start_symbol, start_path = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _) -> nt, [nt, Some 0]
  | ProdRule (nt, _) -> nt, [nt, Some 0]
  in 

  (* Solver object *)
  let solver = initialize_cvc5 () in
  let starting_depth_limit = 7 in 
  try

  (*** Set up the key data structures ***)
  let assertion_level = ref 0 in 
  (* Incremental construction of output term so far *)
  let derivation_tree = ref (Node ((start_symbol, Some 0), ref Utils.IntSet.empty, start_path, 0, ref [])) in 
  (* Tree nodes left to explore *)
  let frontier = ref (DTSet.singleton derivation_tree) in 
  (* Track declared (SMT-level) variables to avoid redeclaration *)
  let declared_variables = ref Utils.StringSet.empty in 
  (* Keep around constraints we may not need to assert *)
  let constraints_to_assert = ref ConstraintSet.empty in 
  (* Set of paths through the tree to help determine when to push constraints from constraints_to_assert *)
  (* let provenance_list = _ in *) (* Challenge: backtracking affects provenance list *)
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  let decision_stack : derivation_tree ref Stack.t ref = ref (B.Stack.create ()) in 
  (* IDS depth limit *) 
  let depth_limit = ref starting_depth_limit in
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  let backtrack_depth = ref false in 

  let exit_flag = ref true in 
  let result = ref None in 

  (* exit flag allows us to toggle between infinite looping (multiple solutions mode) 
     or stopping after one solution *)
  while !exit_flag do 
  (* we start at decision level 1 so we can undo all pushed assertions when restarting *)
  new_decision_level solver assertion_level; 

  (* Iteratively expand the frontier and instantiate the derivation tree leaves *)
  while not (is_complete !derivation_tree) do 
    (* It is unclear how to efficiently update the frontier with backtracking in general. 
    It seems straightforward with DPLL-style backtracking, but not backjumping. So for now, 
    we compute it naively. *)
    frontier := compute_new_frontier !derivation_tree;

    if !Flags.debug then Format.fprintf Format.std_formatter "------------------------\n";
    if !Flags.debug then Format.fprintf Format.std_formatter "Constraints to assert: %a\n"
      (Lib.pp_print_list A.pp_print_expr " ") (ConstraintSet.elements !constraints_to_assert);
    if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
      pp_print_derivation_tree !derivation_tree;
    if !Flags.debug then Format.fprintf Format.std_formatter "Frontier: %a\n"
      (Lib.pp_print_list pp_print_derivation_tree "; ") (DTSet.elements !frontier |> List.map (fun p -> !p));

    (* Choose a node to expand from the frontier *)
    let node_to_expand = 
      (* If possible, take a forced "decision" *)
      let forced_node = List.find_opt (fun element -> match !element with 
      | Node ((nt, _), visited, _, _, _) -> 
          let num_options = List.find_map (fun element -> match element with 
          | A.TypeAnnotation (nt2, _, _) -> 
            if String.lowercase_ascii nt = String.lowercase_ascii nt2 then Some 1 else None  
          | A.ProdRule (nt2, rhss) -> 
            if String.lowercase_ascii nt = String.lowercase_ascii nt2 then Some (List.length rhss) else None 
          ) ast in 
          let num_options = match num_options with 
          | Some num_options -> num_options 
          | None -> Utils.crash ("Failed to find matching production rule or type annotation for " ^ nt)
          in
          num_options - (Utils.IntSet.cardinal !visited) <= 1
      | _ -> true
      ) (DTSet.elements !frontier) in 
      match forced_node with 
      | Some node -> node 
      | None -> DTSet.choose !frontier 
    in

    (* Expand the chosen frontier node *)
    match !node_to_expand with 
    | SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ | ConcreteSetLeaf _ 
    | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
    | ConcretePlaceholderLeaf _ | ConcreteStringLeaf _  -> () (* nothing else to do *)
    | Node (nt, _, _, _, children) when not (List.length !children = 0) -> 
      Utils.crash ("Trying to expand node " ^ (fst nt) ^ " that already has children in dpll")
    | Node (nt, visited, path, depth, children) as node -> 
      if depth > !depth_limit then (
        backtrack_depth := true;
        Utils.debug_print Format.pp_print_string Format.std_formatter "Exceeded depth limit!\n";
        backtrack_decision_level solver assertion_level; 
        backtrack_derivation_tree decision_stack start_symbol depth_limit derivation_tree 
                                  frontier constraints_to_assert declared_variables backtrack_depth;
      ) else 

      let path' = string_of_path path |> String.lowercase_ascii in
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> String.equal (fst nt) nt2
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
            backtrack_decision_level solver assertion_level;
            backtrack_derivation_tree decision_stack start_symbol depth_limit derivation_tree 
                                      frontier constraints_to_assert declared_variables backtrack_depth;
          )
        | A.Dependency _ -> ()
        ) scs;
      | A.ProdRule (_, rhss) -> 
        (* At every real choice, increase the decision level *)
        if (List.length rhss - Utils.IntSet.cardinal !visited) > 1 then (
          new_decision_level solver assertion_level;
          B.Stack.push (ref node) !decision_stack;
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
          | A.Nonterminal (nt, idx_opt) ->
            Node ((nt, idx_opt), ref Utils.IntSet.empty, path @ [nt, idx_opt], depth + 1, ref [])  
          | StubbedNonterminal (_, stub_id) -> DependentTermLeaf stub_id
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
            backtrack_decision_level solver assertion_level;
            backtrack_derivation_tree decision_stack start_symbol depth_limit derivation_tree 
                                      frontier constraints_to_assert declared_variables backtrack_depth;
          ); 
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
    backtrack_decision_level solver assertion_level;
    backtrack_derivation_tree decision_stack start_symbol depth_limit derivation_tree 
                              frontier constraints_to_assert declared_variables backtrack_depth;
  ); 

  (* If we exited the loop, there must be a model. If there was no model, 
     we would have backtracked, and the derivation tree would still be open, 
     so the loop would continue. *)
  let model = match model with 
  | Ok model -> model 
  | Error () -> Utils.crash "internal error; expected a model but got none." in 

  derivation_tree := fill_unconstrained_nonterminals !derivation_tree;
  (* Convert to sygus AST for later processing in the pipeline *)
  let r = sygus_ast_of_derivation_tree !derivation_tree in 
  result := Some r;
  exit_flag := false;
  if !Flags.multiple_solutions then (
    exit_flag := true;
    SA.pp_print_sygus_ast Format.std_formatter r; 
    Format.pp_print_flush Format.std_formatter ();

    (* prepare to generate another solution *)
    initialize_globals derivation_tree start_symbol frontier constraints_to_assert decision_stack declared_variables backtrack_depth; 
    depth_limit := starting_depth_limit;
    issue_solver_command "(pop 1)" solver; 
    push_blocking_clause model declared_variables solver;
  );
  ()
  done; 

  Option.get !result 

  with Failure _ -> 
    StrLeaf "infeasible"

