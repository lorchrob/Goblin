module A = Ast
module SA = SolverAst
module B = Batteries

let (let*) = Res.(>>=)

(* TODO

   * Not the best way to handle declarations for activation literals yet,
     because we may encounter an activation literal at a constraint first, or at
     a leaf first. Don't we already have this problem with regular leaf variables?
     If already declared they won't be redeclared because we check the set of
     declared variables. But maybe can do better (let cvc5 do the check for us?
     and use print-success?)

   * Optimization: normalize in cases where you have an open leaf 
     and multiple prod rule options, but only one remaining option 
   * Optimization: don't represent search tree explicitly in memory; 
     it exists logically but only use minimal bits to represent it 
     (e.g., a map from DTs to indices of expansions you've tried; 
     map from DT to depth, and so on) 
   * Optimization: Better way to do dot notation constraints -- 
     synthesized attribute style? But maybe doesn't work 
     since <A>.<B> could reference multiple <B>s in the same prod rule, 
     and we don't want to use list map. 
     CLP style, but w/ semantics to somehow avoid list map?
     W/ CLP style variable passing, the user specifies exactly what gets passed up the chain... 
   * Optimization: Actually for dot notation, one idea: Do "constraint passing", where instead 
     of dealing with dot notation constraint at the given parent node, move it to the child 
     node in the DT. Seems impossible if you have something like <A>.<B> = <C>.<D>, where both 
     <B> and <D> may not occur. But in this case, you can create a fresh literal 
     fresh_lit, and generate the constraints <C>.<D> = fresh_lit and <A>.<B> = fresh_lit. 
     Then, you can pass these constraints down. To support this, you'd also need to 
     reference the other way in dot notation -- e.g., you'd pass down <D> = parent.fresh_lit 
     to child node <C>.
*)

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

Interfacing with the solver
    * Flatten each NT name, all the way from root, with indices to disambiguate. 
      E.g., root.nt1[0].nt2[1].leaf]
    * Parse the model into an AST (with each new variable, incrementally expand the tree)
    * Serialize the AST
*) 

(* Raised when the search proves that the grammar has no solution *)
exception Infeasible_grammar

(* SMT model: values of declared SMT variables *)
type model = Value.t Utils.StringMap.t

type path = (Nt.t * int option * int option) list

(* Derivation tree (a possibly unfinished generated term).
   Each node is identified by its path from the root; its label is the last element of the path. *)
type derivation_tree = {
  path : path;
  expansion : expansion;
}
and expansion = 
(* Not yet expanded *)
| Open
(* Expanded with a production rule *)
| Children of derivation_tree list
(* Symbolic terminal of the given type, with its value (if already known) *)
| Terminal of A.il_type * Value.t option
(* Computed separately (derived field or divide and conquer subproblem) *)
| Dependent of Nt.stub

let label dt = Utils.last dt.path

(* A spot in the search: the derivation tree, its search depth, 
   and the (indices of the) expansions already tried from here *)
type search_node = {
  dt : derivation_tree;
  depth : int;
  tried : int list ref;
}

let rec pp_print_derivation_tree ppf dt = 
  let pp_print_label ppf = function
  | (nt, Some idx1, Some idx2) -> Format.fprintf ppf "%a.%d.%d" Nt.pp_symbol nt idx1 idx2
  | (nt, _, _) -> Nt.pp_symbol ppf nt
  in
  match dt.expansion with 
  | Open -> Format.fprintf ppf "(%a )" pp_print_label (label dt)
  | Children children -> 
    Format.fprintf ppf "(%a %a)"
      pp_print_label (label dt)
      (Lib.pp_print_list pp_print_derivation_tree " ") children
  | Terminal (_, None) -> Format.fprintf ppf "(%a sym_leaf)" pp_print_label (label dt)
  | Terminal (_, Some value) -> Format.fprintf ppf "(%a %a)" pp_print_label (label dt) Value.pp value
  | Dependent _ -> Format.fprintf ppf "(%a dep_sym_leaf)" pp_print_label (label dt)

module ConstraintSet = Set.Make(struct
  type t = A.expr
  let compare = Stdlib.compare
end)

(* Constraints are asserted the moment they are encountered, at the current
   assertion level, so that backtracking retracts them via (pop 1). *)
let assert_constraints: Smt.solver_instance -> ConstraintSet.t -> unit
= fun solver constraints ->
  ConstraintSet.iter (Smt.assert_smt_constraint solver) constraints

let random_int_in_range: int -> int -> int
= fun min max ->
  min + Random.int (max - min + 1) 

let declare_smt_variables 
= fun variable_stack declared_variables ctx solver blocking_clause_vars assertion_level -> 
  Utils.StringMap.iter (fun var ty -> 
    if Utils.StringSet.mem var !declared_variables then 
      () 
    else 
      let declaration_string = Format.asprintf "(declare-fun %s () %a)\n" 
        var SmtPrinter.pp_print_ty ty 
      in
      declared_variables := Utils.StringSet.add var !declared_variables;
      (* Hacky -- if at the zeroth assertion level, we keep variables around 
         by storing them in blocking_clause_vars, 
         even if they aren't part of a blocking clause *)
      if !assertion_level = 0 then 
        blocking_clause_vars := Utils.StringSet.add var !blocking_clause_vars; 
      let top = Stack.top variable_stack in 
      top := Utils.StringSet.add var !top;
      Smt.issue_solver_command declaration_string solver
  ) ctx 

(* State expression nonterminals in terms of absolute paths from 
   the root of the derivation tree *)
let rec universalize_expr: bool -> (Nt.t * int option * int option) list -> Ast.expr -> Ast.expr
= fun is_type_annotation prefix expr ->
  let r = universalize_expr is_type_annotation prefix in
  match expr with
  | A.NTExpr (nts, p) -> 
    (* In the derivation tree structure, type annotation NTs have a duplicate at the end of the path. 
       Remove it. *)
    let prefix = if is_type_annotation then Utils.init prefix else prefix in
    A.NTExpr (prefix @ nts, p)
  | ActLit (nt_expr, p) -> ActLit (r nt_expr, p)
  | BVCast (len, expr, p) -> BVCast (len, r expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (r expr1, op, r expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, r expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (r expr1, op, r expr2, p) 
  | Singleton (expr, p) -> Singleton (r expr, p)
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map r exprs, p) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ 
  | EmptySet _ -> expr
  | InhAttr _ 
  | OwnSynthAttr _
  | SynthAttr _ -> assert false

let string_of_path path = 
  let path = List.map (fun (nt, idx1, idx2) -> match idx1, idx2 with 
  | None, None -> Nt.to_symbol nt
  | Some idx1, Some idx2 -> Format.asprintf "%a!%d!%d" Nt.pp_symbol nt idx1 idx2
  | _ -> assert false
  ) path in 
  String.concat "_" path

(* Unexpanded children of the node at `path`, for production rule option `ges` *)
let children_of_ges path ges = List.map (fun ge -> match ge with 
| A.Nonterminal (nt, idx_opt1, idx_opt2, _, _) ->
  { path = path @ [nt, idx_opt1, idx_opt2]; expansion = Open }
| StubbedNonterminal stub -> 
  { path = path @ [stub.stands_for, None, None]; expansion = Dependent stub }
) ges

(* Normalize a derivation tree for a fixed spot in the search tree.
   Assert the associated constraints discovered during normalization. *)
let rec normalize_derivation_tree ctx ast declared_variables solver 
                                  variable_stack blocking_clause_vars assertion_level dt =
let r = normalize_derivation_tree ctx ast declared_variables solver variable_stack
                                   blocking_clause_vars assertion_level in 
let path = dt.path in
match dt.expansion with 
| Open -> 
  let (nt, idx1, idx2) = label dt in
  let forced_expansion = List.find_map (fun element -> match element with 
  | A.ProdRule (nt2, _, [Rhs (ges, scs, _, _)], _) -> 
    if Nt.equal_ci nt nt2 then 
      let constraints_to_add, _exprs = List.concat_map (fun sc -> match sc with 
      | A.SmtConstraint (e, _) -> [universalize_expr false path e, e] 
      | DerivedField _ -> [] 
      | AttrDef _ -> assert false
      ) scs |> List.split in
      let expr_variables = List.map A.get_nts_from_expr2 constraints_to_add |> List.flatten in
      let ty_ctx = List.fold_left (fun acc nt -> 
        let ty = Nt.Map.find_opt (List.rev nt |> List.hd |> Utils.tr_fst) ctx in 
        let ty = match ty with 
        | Some ty -> ty 
        | None -> Utils.crash (Format.asprintf "couldn't find %a" Nt.pp (List.rev nt |> List.hd |> Utils.tr_fst))
        in
        let str = Format.asprintf "%a" 
          (Lib.pp_print_list SmtPrinter.pp_print_nt_helper "_") nt 
        in
        (* Declare variable and its activation literal *)
        Utils.StringMap.add str ty (Utils.StringMap.add (str ^ "_actlit") A.Bool acc) 
      ) Utils.StringMap.empty expr_variables in
      declare_smt_variables variable_stack declared_variables ty_ctx solver blocking_clause_vars assertion_level ;
      assert_constraints solver (ConstraintSet.of_list constraints_to_add);
      Some (Children (children_of_ges path ges))
    else 
      None
  | A.ProdRule (_, _, _, _) -> None 
  | TypeAnnotation (nt2, ty, scs, p) ->
    let path' = string_of_path path |> String.lowercase_ascii in
    if Nt.equal_ci nt nt2 then 
      let constraints_to_add = List.concat_map (fun sc -> match sc with 
      | A.SmtConstraint (e, _) -> 
      declare_smt_variables variable_stack declared_variables (Utils.StringMap.singleton path' ty) solver blocking_clause_vars assertion_level;
        [universalize_expr true path e] 
      | DerivedField _ -> [] 
      | AttrDef _ -> assert false
      ) scs |> ConstraintSet.of_list in
      (* Also declare and assert activation literals *)
      let path'' = (string_of_path path |> String.lowercase_ascii) ^ "_actlit" in
      declare_smt_variables variable_stack declared_variables 
        (Utils.StringMap.singleton path'' A.Bool) solver blocking_clause_vars assertion_level;
      let actlit = 
        universalize_expr true path (A.ActLit (A.NTExpr ([nt2, idx1, idx2], p), p))
      in
      assert_constraints solver (ConstraintSet.add actlit constraints_to_add);
      Some (Terminal (ty, None))
    else None 
  ) ast in 
  (match forced_expansion with 
  | Some (Children children) -> { dt with expansion = Children (List.map r children) }
  | Some expansion -> { dt with expansion }
  | None -> dt)
| Children children -> 
  { dt with expansion = Children (List.map r children) }
| Terminal _ | Dependent _ -> dt

let new_decision_level: Smt.solver_instance -> int ref -> Utils.StringSet.t ref Stack.t ref -> unit 
= fun solver assertion_level variable_stack ->
  let push_cmd = Format.asprintf "(push 1)" in
  Stack.push (ref Utils.StringSet.empty) (!variable_stack);
  assertion_level := !assertion_level + 1;
  Smt.issue_solver_command push_cmd solver; 
  ()

let initialize_globals ctx ast derivation_tree start_symbol
                       decision_stack _declared_variables backtrack_depth curr_st_node declared_variables solver 
                       variable_stack blocking_clause_vars assertion_level = 
  (* Incremental construction of output term so far *)
  derivation_tree := { path = [start_symbol, Some 0, Some 0]; expansion = Open };
  derivation_tree := normalize_derivation_tree ctx ast declared_variables solver !variable_stack blocking_clause_vars assertion_level !derivation_tree ;
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  decision_stack := B.Stack.create ();
  variable_stack := B.Stack.create ();
  Stack.push (ref Utils.StringSet.empty) !variable_stack;
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  backtrack_depth := false; 
  (* Current spot in the search tree *) 
  curr_st_node := { dt = !derivation_tree; depth = 0; tried = ref [] };
  declared_variables := !blocking_clause_vars ;
  ()

(* We (1) pick an NT to expand uniformly at random, then 
      (2) pick a production rule option based on user distribution (uniform if absent)
*) 
let sample_excluding (expansion_probs : float list list) (visited : int list) : int =
  (* Annotate probabilities with indices *) 
  let expansion_probs, _ = List.fold_left (fun (acc_list, acc_i) probs -> 
    let probs, acc_i = List.fold_left (fun (acc_probs, acc_i) prob -> 
      acc_probs @ [prob, acc_i], acc_i + 1 
    ) ([], acc_i) probs in 
    acc_list @ [probs], acc_i 
  ) ([], 0) expansion_probs in

  (* Eliminate visited indices *) 
  let expansion_probs = List.map (fun probs -> 
    List.filter (fun (_, idx) -> not (List.mem idx visited)) probs 
  ) expansion_probs in

  (* Remove nonterminals with no expansion options *) 
  let expansion_probs = List.filter (fun l -> not (List.is_empty l)) expansion_probs in

  (* Pick a nonterminal to expand uniformly at random *)
  let node_choice_idx = Random.int (List.length expansion_probs) in
  let node_probs = List.nth expansion_probs node_choice_idx in

  (* Step 2: normalize probabilities within the node *)
  let total = List.fold_left (fun acc (p, _) -> acc +. p) 0.0 node_probs in
  let normalized = List.map (fun (p, i) -> (p /. total, i)) node_probs in

  (* Step 3: sample an RHS index according to normalized probabilities *)
  let r = Random.float 1.0 in
  let rec pick acc = function
    | [] -> Utils.crash "sample_excluding: rounding error"
    | (p, i) :: rest ->
        let acc = acc +. p in
        if r <= acc then i else pick acc rest
  in
  pick 0.0 normalized

(* Return expanded node, updated DT, whether or not it was a real choice *)
let find_new_expansion ast derivation_tree curr_st_node = 
  let visited_indices = !(!curr_st_node.tried) in 
  let rec expansion_probabilities dt =
  match dt.expansion with 
  | Open ->
    let (nt, _, _) = label dt in
    let probs =
    match List.find_opt (fun e -> match e with
    | A.ProdRule (nt2, _, _, _) | TypeAnnotation (nt2, _, _, _) -> nt = nt2
    ) ast with
    | Some (ProdRule (_, _, rhss, _)) ->
      List.map (function A.Rhs (_, _, Some p, _) -> p | _ -> 1.0 /. (float_of_int (List.length rhss))) rhss
    | Some (TypeAnnotation _) -> [1.0]
    | None -> Utils.crash "No matching grammar rule"
    in
    [probs]
  | Children children -> 
    List.flatten (List.map expansion_probabilities children)
  | Terminal _ | Dependent _ -> []
  in
  let num_expansions dt = List.length (List.flatten (expansion_probabilities dt)) in
  (* Perform the nth expansion (counting over the open nodes, in order) *)
  let rec perform_nth_expansion dt n = 
  match dt.expansion with 
  | Children children -> 
    let expanded_node, children = perform_nth_expansion_list children n in 
    expanded_node, { dt with expansion = Children children }
  | Open -> 
    let (nt, _, _) = label dt in
    let element = List.find (fun element -> match element with 
    | A.TypeAnnotation (nt2, _, _, _) 
    | ProdRule (nt2, _, _, _) -> Nt.equal_ci nt nt2
    ) ast in 
    let expansion = match element with 
    | TypeAnnotation (_, ty, _, _) -> 
      if n = 1 then Terminal (ty, None) else assert false 
    | ProdRule (_, _, rhss, _) -> (
      match List.nth rhss n with 
      | A.Rhs (ges, _, _, _) -> Children (children_of_ges dt.path ges)
      | StubbedRhs stub -> Dependent stub
    ) in
    let expanded_node = { dt with expansion } in
    expanded_node, expanded_node 
  | Terminal _ | Dependent _ -> assert false 
  and perform_nth_expansion_list children n = match children with 
  | child :: children -> 
    let m = num_expansions child in 
    if m > n then 
      let expanded_node, child = perform_nth_expansion child n in 
      expanded_node, child :: children
    else 
      let expanded_node, children = perform_nth_expansion_list children (n - m) in 
      expanded_node, child :: children
  | [] -> assert false
  in
  let expansion_probabilities_list = expansion_probabilities derivation_tree in 
  let total_num_choices = expansion_probabilities_list |> List.concat |> List.length in
  let index_to_pick = sample_excluding expansion_probabilities_list visited_indices in  
  let expanded_node, new_dt = perform_nth_expansion derivation_tree index_to_pick in 
  let real_choice = total_num_choices - (List.length visited_indices) > 1 in 
  expanded_node, index_to_pick, new_dt, real_choice 

let backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
              depth_limit start_symbol derivation_tree curr_st_node
              variable_stack blocking_clause_vars = 
  if !assertion_level = 1 then ( (* restarting *)
    (*Format.pp_print_string Format.std_formatter "Restarting...\n%!";*)
    Smt.issue_solver_command "(pop 1)" solver; 
    Smt.issue_solver_command "(push 1)" solver;
    (if not !backtrack_depth then raise Infeasible_grammar);
    depth_limit := !depth_limit + 1;
    if !Flags.debug then Format.fprintf Format.std_formatter "Increasing depth limit to %d\n" !depth_limit;
    initialize_globals ctx ast derivation_tree start_symbol 
                       decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver
                       variable_stack blocking_clause_vars assertion_level; 
  ) else ( 
    assertion_level := !assertion_level - 1;
    Smt.issue_solver_command "(pop 1)" solver; 
    let st_node = Stack.pop !decision_stack in
    let popped_vars = Stack.pop !variable_stack in 
    let dt = st_node.dt in 
    declared_variables := 
      Utils.StringSet.union
        (Utils.StringSet.diff !declared_variables !popped_vars)
        !blocking_clause_vars;
    derivation_tree := dt; 
    curr_st_node := st_node
  )

let model_of_solver_ast: SolverAst.solver_ast -> (model, unit) result
= fun solver_ast -> 
  match solver_ast with 
  | Infeasible -> 
    Format.pp_print_flush Format.std_formatter (); Error ()
  | Model values -> 
    Ok (List.fold_left (fun acc (symbol, value) -> match value with 
      | Value.Unit -> acc
      | Bool _ | Int _ | String _ | Placeholder _ | BitVector _ | BitList _ | StringSet _ -> 
        Utils.StringMap.add symbol value acc
    ) Utils.StringMap.empty values)
  | Leaf _ | StubLeaf _ | Node _ -> Utils.crash "Unexpected case in model_of_solver_ast"

let get_smt_result: A.ast -> Smt.solver_instance -> bool -> (model, unit) result option
= fun ast solver get_model -> 
  Smt.issue_solver_command "(check-sat)\n" solver;
  let response = Smt.read_check_sat_response solver in
  if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
  if response = "sat" && get_model then (
    Smt.issue_solver_command "(get-model)\n" solver;
    let response = Smt.read_get_model_response solver in
    if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
    let result = match Parsing.parse_solver response ast with 
    | Ok result -> result 
    | Error msg -> Format.fprintf Format.std_formatter "Error parsing: %s\n" msg; assert false 
    in
    Some (model_of_solver_ast result)
  ) else if response = "sat" then None  
  else
    let result = match Parsing.parse_solver response ast with 
    | Ok result -> result 
    | Error msg -> Format.fprintf Format.std_formatter "Error parsing: %s\n" msg; assert false 
    in
    Some (model_of_solver_ast result)

(* Set each terminal's value from the model *)
let rec instantiate_terminals: model -> derivation_tree -> derivation_tree 
= fun model dt -> 
  match dt.expansion with 
  | Terminal (ty, _) -> 
    let symbol = string_of_path dt.path |> String.lowercase_ascii in
    (* If the model does not contain the variable, it is unconstrained *)
    { dt with expansion = Terminal (ty, Utils.StringMap.find_opt symbol model) }
  | Children children -> 
    { dt with expansion = Children (List.map (instantiate_terminals model) children) }
  | Open | Dependent _ -> dt
  
(* Assign random values to the terminals that the model leaves unconstrained *)
let rec fill_unconstrained_nonterminals: derivation_tree -> derivation_tree 
= fun dt -> 
  let value_of_ty: A.il_type -> Value.t = function
  | Unit -> Unit
  | Int -> Int (random_int_in_range (-100) 100)
  | Bool -> Bool (Random.bool ())
  | BitList -> BitList (Utils.random_bools (random_int_in_range 0 25))
  | BitVector n -> BitVector (n, Utils.random_bools n)
  | Placeholder -> Placeholder "generated_placeholder"
  | String -> String (Utils.random_string (random_int_in_range 0 25))
  | Set String -> StringSet Utils.StringSet.empty
  | Set _ -> Utils.crash "TODO: Support more set types in DPLL module"
  | ADT _ -> Utils.crash "Unexpected case in fill_unconstrained_nonterminals"
  in
  match dt.expansion with 
  | Terminal (ty, None) -> { dt with expansion = Terminal (ty, Some (value_of_ty ty)) }
  | Children children -> 
    { dt with expansion = Children (List.map fill_unconstrained_nonterminals children) }
  | Terminal (_, Some _) | Open | Dependent _ -> dt

let rec is_complete dt = match dt.expansion with
| Open -> false
| Terminal _ | Dependent _ -> true
| Children children -> 
  children <> [] && List.for_all is_complete children 

let rec solver_ast_of_derivation_tree: derivation_tree -> SA.solver_ast 
= fun dt -> 
  let children = match dt.expansion with 
  | Open -> []
  | Children children -> List.map solver_ast_of_derivation_tree children
  | Terminal (_, Some value) -> [Leaf value]
  (* TODO: Unfilled terminals are rendered as a placeholder made of the path's symbols *)
  | Terminal (_, None) -> 
    let symbols = List.map (fun (nt, _, _) -> Nt.to_symbol nt) (dt.path @ [label dt]) in
    [Leaf (Placeholder (String.concat "" symbols))]
  | Dependent stub -> [StubLeaf stub]
  in
  Node (label dt, children)

let pp_print_model_pair ppf (k, v) = 
  Format.fprintf ppf "(= %s %a)" 
    k 
    Value.pp_smt v 

(* Names of the terminal variables in `dt`. 
   TODO: The separators differ from `string_of_path`, so these never match model variables *)
let rec get_dt_vars dt = 
  let id_str = match label dt with 
  | (id, Some idx1, Some idx2) -> Format.asprintf "%a.%d.%d" Nt.pp_symbol id idx1 idx2
  | (id, None, None) -> Nt.to_symbol id 
  | _ -> assert false
  in
  let r = match dt.expansion with 
  | Open | Dependent _ -> Utils.StringSet.empty
  | Terminal _ -> Utils.StringSet.singleton ""
  | Children children -> 
    List.fold_left Utils.StringSet.union Utils.StringSet.empty (List.map get_dt_vars children)
  in
  Utils.StringSet.map (fun child -> 
    if String.equal child "" then id_str 
    else id_str ^ "_" ^ child
  ) r

let push_blocking_clause variable_stack model dt declared_variables solver blocking_clause_vars assertion_level update_bc_vars = 
  let dt_vars = get_dt_vars !dt in
  let dt_vars = Utils.StringSet.map (fun s -> String.lowercase_ascii s) dt_vars in
  (*Format.fprintf Format.std_formatter "dt_vars: %a, model vars: %a\n" 
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringSet.to_list dt_vars)  
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringMap.bindings model |> List.map fst); *)
  let model = Utils.StringMap.filter (fun var _ -> Utils.StringSet.mem var dt_vars) model in
  let ctx = Utils.StringMap.map Value.ty model in 
  declare_smt_variables variable_stack declared_variables ctx solver blocking_clause_vars assertion_level; 
  if update_bc_vars then 
    blocking_clause_vars := Utils.StringSet.union !blocking_clause_vars 
     (Utils.StringMap.bindings model |> List.map fst |> Utils.StringSet.of_list);
  let blocking_clause_str = 
    if Utils.StringMap.cardinal model > 0 then 
      Format.asprintf "(assert (not (and %a)))" 
      (Lib.pp_print_list pp_print_model_pair " ") (Utils.StringMap.bindings model)
    else "(assert true)"
  in
  Smt.issue_solver_command blocking_clause_str solver  

let rec generate_n_solutions n ast model r derivation_tree declared_variables solver blocking_clause_vars variable_stack assertion_level = 
  if n = 1 then 
    [model, r] 
  else (
    push_blocking_clause variable_stack model derivation_tree declared_variables solver blocking_clause_vars assertion_level false;
    let model2 = get_smt_result ast solver true in   
    match model2 with 
    | Some (Ok model2) -> (* sat *)
      if !Flags.debug then Format.pp_print_string Format.std_formatter "it was SAT, instantiating in derivation tree\n"; 
      derivation_tree := instantiate_terminals model2 !derivation_tree; 
      let r2 = solver_ast_of_derivation_tree !derivation_tree in 
      (model, r) :: (generate_n_solutions (n-1) ast model2 r2 derivation_tree declared_variables solver blocking_clause_vars variable_stack assertion_level) 
    | None  
    | Some (Error ()) -> 
      [model, r]
  )
   

(*
  * Maintain a current DT and current search tree node
  * Pick an open, unexplored expansion of DT
  * Expand DT and record it as a child of the current search tree node 
  * Update current search tree node 
  * Collect the associated constraints with this grammar element 
  * Assert and remove all applicable constraints, and record the non-applicable ones
  * If this is a real choice, push an assertion level 
  * Assert applicable constraints 
    * If SAT  
      * If DT is complete, instantiate, push blocking clause, continue 
    * If UNSAT 
      * Pop an assertion level, backtrack in search tree to last real choice 
      * Remove the constraints from the constraint set associated with nodes no longer in DT

*) 
let dpll: TypeChecker.context -> A.semantic_constraint Nt.StubMap.t -> A.ast -> SA.solver_ast
= fun ctx dep_map ast ->  
  let _ = match !Flags.seed with 
  | None -> 
    Random.self_init ()
  | Some seed -> 
    Random.init seed
  in

  let start_symbol, start_path = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _, _) -> nt, [nt, Some 0, Some 0]
  | ProdRule (nt, _, _, _) -> nt, [nt, Some 0, Some 0]
  in 

  (* Solver object *)
  let solver = Smt.initialize_solver () in
 
  (* IDEA: dynamically alter starting depth limit *)
  (*** HYPERPARAMETERS *)
  let starting_depth_limit = !Flags.starting_depth_limit in 
  let restart_rate = !Flags.restart_rate in 
  let sols_per_iter = !Flags.sols_per_iter in
  let num_solutions_to_find = !Flags.num_solutions in 

  let num_solutions = ref 0 in 
  let num_iterations = ref 0 in
  try

  (*** Set up the key data structures ***)
  (* Bookkeeping for declared variables *) 
  let variable_stack : (Utils.StringSet.t ref) Stack.t ref = ref (B.Stack.create ()) in
  Stack.push (ref Utils.StringSet.empty) !variable_stack;
  let blocking_clause_vars = ref Utils.StringSet.empty in
  let assertion_level = ref 0 in 
  (* Track declared (SMT-level) variables to avoid redeclaration *)
  let declared_variables = ref Utils.StringSet.empty in 
  (* Incremental construction of output term so far *)
  let derivation_tree = ref { path = start_path; expansion = Open } in 
  derivation_tree := normalize_derivation_tree ctx ast declared_variables solver !variable_stack blocking_clause_vars assertion_level !derivation_tree ;
  (* Current spot in the search tree *) 
  let curr_st_node = ref { dt = !derivation_tree; depth = 0; tried = ref [] } in
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  let decision_stack : search_node Stack.t ref = ref (B.Stack.create ()) in 
  (* IDS depth limit *) 
  let depth_limit = ref starting_depth_limit in
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  let backtrack_depth = ref false in 

  (* Variables for multiple-solutions flag *)
  let exit_flag = ref true in 
  let result = ref None in 

  (* we start at decision level 1 so we can undo all pushed assertions when restarting *)
  new_decision_level solver assertion_level variable_stack; 
  (*Stack.push !curr_st_node !decision_stack;*)

  (* exit flag allows us to toggle between infinite looping (multiple solutions mode) 
     or stopping after one solution *)
  while !exit_flag do 
  while not (is_complete !derivation_tree) do
    (*Format.printf "Declared variables: %d\n" (Utils.StringSet.cardinal !declared_variables); 
    Format.printf "Assertion level: %d\n" !assertion_level; *)
    Format.pp_print_flush Format.std_formatter () ;
    num_iterations := !num_iterations + 1;
    (*Format.printf "num_iterations: %d\n%!" !num_iterations;*)

    (*if !num_iterations mod 100 = 0 then 
      Format.fprintf Format.std_formatter "num_iterations: %d\n" !num_iterations; *)
     
    if !Flags.debug then Format.fprintf Format.std_formatter "------------------------\n";
    if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
      pp_print_derivation_tree !derivation_tree;

    (* Choose an expansion based on the search tree *)
    let expanded_node, expansion_index, new_dt, real_choice = find_new_expansion ast !derivation_tree curr_st_node in
    derivation_tree := new_dt;
    if real_choice then ( 
      new_decision_level solver assertion_level variable_stack; 
      Stack.push !curr_st_node !decision_stack;
    );
    derivation_tree := normalize_derivation_tree ctx ast declared_variables solver !variable_stack blocking_clause_vars assertion_level !derivation_tree ; 
    !curr_st_node.tried := expansion_index :: !(!curr_st_node.tried);
    curr_st_node := { dt = !derivation_tree; depth = !curr_st_node.depth + 1; tried = ref [] }; 

    if !Flags.debug then Format.fprintf Format.std_formatter "Expanded DT: %a\nExpanded node: %a\n" 
      pp_print_derivation_tree !derivation_tree 
      pp_print_derivation_tree expanded_node;

     if !num_iterations = restart_rate then (
        num_iterations := 0;
        (*Format.fprintf Format.std_formatter "Restarting\n";*)
        let pop_cmd = Format.asprintf "(pop %d)" !assertion_level in 
        Smt.issue_solver_command pop_cmd solver; 
        Smt.issue_solver_command "(push 1)" solver;

        (* prepare to generate another solution *)
        assertion_level := 1;
        depth_limit := starting_depth_limit;
        initialize_globals ctx ast derivation_tree start_symbol 
                           decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver
                           variable_stack blocking_clause_vars assertion_level; 
    ) else 

    (* Assert constraints for the expanded node *)
    match expanded_node.expansion with 
    | Open -> assert false
    | Children _ | Terminal _ | Dependent _ -> 
      let nt = label expanded_node in
      let path = expanded_node.path in
      let depth = !curr_st_node.depth in
      if !Flags.debug then Format.fprintf Format.std_formatter "Current search tree depth: %d\n" 
        depth;

      (* Backtrack due to depth limit if necessary *)
      if depth > !depth_limit then (
        backtrack_depth := true;
        Utils.debug_print Format.pp_print_string Format.std_formatter 
          ("Exceeded depth limit " ^ (string_of_int !depth_limit) ^ "!\n");
        backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  depth_limit start_symbol derivation_tree curr_st_node 
                  variable_stack blocking_clause_vars
      ) else 

      (* Find the associated AST rule for the new expansion *)
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _, _, _) 
      | A.TypeAnnotation (nt2, _, _, _) -> Nt.equal_ci (Utils.tr_fst nt) nt2
      ) ast in 
      let path' = string_of_path path |> String.lowercase_ascii in

      (* Assert semantic constraints for the new expansion (backtrack if necessary) *)
      match grammar_rule with 
      | A.TypeAnnotation (_, _, [], _) -> ()
      | A.TypeAnnotation (_, ty, scs, p) -> 
        List.iter (fun sc -> match sc with 
        | A.SmtConstraint (expr, _) ->
          declare_smt_variables !variable_stack declared_variables (Utils.StringMap.singleton path' ty) solver blocking_clause_vars assertion_level; 
          (* Also declare and assert activation literals *)
          let path'' = (string_of_path path |> String.lowercase_ascii) ^ "_actlit" in
          declare_smt_variables !variable_stack declared_variables 
            (Utils.StringMap.singleton path'' A.Bool) solver blocking_clause_vars assertion_level;
          let actlit = 
            universalize_expr true path (A.ActLit (A.NTExpr ([nt], p), p))
          in
          assert_constraints solver
            (ConstraintSet.of_list [universalize_expr true path expr; actlit]);
          let model = get_smt_result ast solver false in  
          (match model with 
          | Some (Ok _) -> assert false
          | None -> 
            if !Flags.debug then Format.pp_print_string Format.std_formatter 
              "it was SAT, waiting to expand before instantiating in derivation tree\n"; 
          | Some (Error ()) -> 
            backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  depth_limit start_symbol derivation_tree curr_st_node
                  variable_stack blocking_clause_vars
          )
        | A.DerivedField _ -> ()
        | A.AttrDef _ -> assert false
        ) scs;
      | A.ProdRule (_, _, rhss, _) -> 
        (*Format.printf "Finding the chosen rule for %a\n%!" 
          pp_print_derivation_tree expanded_node; *)
        let chosen_rule = List.find (fun rhs -> match rhs, expanded_node.expansion with 
        | A.StubbedRhs stub1, Dependent stub2 -> Nt.equal_stub stub1 stub2
        | A.Rhs (ges, _, _, _), Children children -> 
          List.length ges = List.length children && 
          List.for_all2 (fun child ge -> match ge with 
          | A.Nonterminal (nt2, idx3, idx4, _, _) -> 
            let (nt, idx1, idx2) = label child in
            Nt.equal_ci nt nt2 && idx1 = idx3 && idx2 = idx4
          | A.StubbedNonterminal stub2 -> (
            match child.expansion with 
            | Dependent stub1 -> Nt.equal_stub stub1 stub2
            | Open | Children _ | Terminal _ -> false)
          ) children ges 
        | A.StubbedRhs _, (Open | Children _ | Terminal _) 
        | A.Rhs _, (Open | Terminal _ | Dependent _) -> false 
        ) rhss in
        if !Flags.debug then Format.fprintf Format.std_formatter "Chose rule %a\n" 
          A.pp_print_prod_rule_rhs chosen_rule;
        match chosen_rule with 
        | A.StubbedRhs _ -> () 
        | A.Rhs (_, scs, _, _) -> 
          List.iter (fun sc -> match sc with 
          | A.SmtConstraint (expr, _) ->
            (* Assert semantic constraints for production rules *)
            let expr_variables = A.get_nts_from_expr2 expr in
            let ty_ctx = List.fold_left (fun acc nt -> 
              let ty = Nt.Map.find (List.rev nt |> List.hd |> Utils.tr_fst) ctx in 
              let str = Format.asprintf "%a" (Lib.pp_print_list SmtPrinter.pp_print_nt_helper "_") nt in
              let str = path' ^ "_" ^ str in 
              (* Declare variable and its activation literal *)
              Utils.StringMap.add str ty (Utils.StringMap.add (str ^ "_actlit") A.Bool acc) 
            ) Utils.StringMap.empty expr_variables in
            declare_smt_variables !variable_stack declared_variables ty_ctx solver blocking_clause_vars assertion_level;
            assert_constraints solver
              (ConstraintSet.singleton (universalize_expr false path expr));
            (* don't instantiate yet -- we haven't hit the leaf nodes *)
            (* derivation_tree := instantiate_terminals model derivation_tree;  *)
          | A.DerivedField _ -> ()
          | A.AttrDef _ -> assert false
          ) scs;

          let model = get_smt_result ast solver false in  
          (match model with 
          | None -> (* sat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter 
              "it was SAT, waiting to expand before instantiating in derivation tree\n"; 
          | Some (Error ()) -> (* unsat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
            backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  depth_limit start_symbol derivation_tree curr_st_node 
                  variable_stack blocking_clause_vars
          | Some _ -> assert false
          ); 

  done;

  if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
    pp_print_derivation_tree !derivation_tree;

  (* The derivation tree is complete; get the model that instantiates its terminals *)
  let model = get_smt_result ast solver true in
  (match model with 
  | Some (Ok model) -> (* sat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was SAT, instantiating in derivation tree\n"; 
    derivation_tree := instantiate_terminals model !derivation_tree; 
  | None -> assert false
  | Some (Error ()) -> (* unsat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
    backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  depth_limit start_symbol derivation_tree curr_st_node 
                  variable_stack blocking_clause_vars
  ); 

  (* If we exited the loop, there must be a model. If there was no model, 
     we would have backtracked, and the derivation tree would still be open, 
     so the loop would continue. *)
  let model = match model with 
  | Some (Ok model) -> model 
  | _ -> Utils.crash "internal error; expected a model but got none." in 

  derivation_tree := fill_unconstrained_nonterminals !derivation_tree;
  (* Convert to solver AST for later processing in the pipeline *)
  let r = solver_ast_of_derivation_tree !derivation_tree in 
  result := Some r;
  exit_flag := false;
  if !Flags.multiple_solutions then (
    num_iterations := 0;
    num_solutions := !num_solutions + sols_per_iter;
    exit_flag := (!num_solutions <= num_solutions_to_find) || num_solutions_to_find = (-1);

    if !Flags.debug then Format.fprintf Format.std_formatter "Generating %d solutions\n" sols_per_iter;
    let models, rs = generate_n_solutions sols_per_iter ast model r derivation_tree declared_variables 
                solver blocking_clause_vars !variable_stack assertion_level |> List.split in 

    (* Compute dependencies and output *)
    Format.pp_print_flush Format.std_formatter ();
    let () = List.iter (fun r -> 
      let r = ComputeDeps.compute_deps dep_map ast r in
      Format.fprintf Format.std_formatter "$\n%a" 
        SA.pp_print_solver_ast r;
    ) rs in
    Format.pp_print_flush Format.std_formatter ();

    (* Need to pop all the way back to zeroth level so we can assert persisting blocking clause *)
    let pop_cmd = Format.asprintf "(pop %d)" !assertion_level in 
    Smt.issue_solver_command pop_cmd solver; 
    declared_variables := !blocking_clause_vars;
    variable_stack :=  B.Stack.create () ;
    Stack.push (ref Utils.StringSet.empty) !variable_stack ;
    if !Flags.debug then Format.fprintf Format.std_formatter "Pushing blocking clause\n" ;
    List.iter (fun model -> 
      push_blocking_clause !variable_stack model derivation_tree declared_variables solver blocking_clause_vars assertion_level true
    ) models ;
    Smt.issue_solver_command "(push 1)" solver;

    (* prepare to generate another solution *)
    assertion_level := 1;
    depth_limit := starting_depth_limit;
    initialize_globals ctx ast derivation_tree start_symbol 
                       decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver 
                       variable_stack blocking_clause_vars assertion_level; 
  ); 
  ()
  done; 

  Smt.cleanup_solver solver;
  Option.get !result 

  with 
  | Infeasible_grammar -> 
    Smt.cleanup_solver solver;
    Format.pp_print_flush Format.std_formatter ();
    Infeasible
  | Failure e -> 
    Smt.cleanup_solver solver;
    Utils.crash e
