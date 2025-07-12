module A = Ast
module SA = SygusAst
module B = Batteries

let (let*) = Res.(>>=)
(* TODO 

   * Optimization: normalize in cases where you have an open leaf and multiple prod rule options, 
     but only one remaining option 
   * Optimization: don't represent search tree explicitly in memory; it exists logically but only use 
     minimal bits to represent it (e.g., a map from DTs to indices of expansions you've tried; 
     map from DT to depth, and so on) 
   * Optimization: Better way to do dot notation constraints -- synthesized attribute style? But maybe doesn't work 
     since <A>.<B> could reference multiple <B>s in the same prod rule, and we don't want to use list map. 
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
   * Optimization: Track constraints you can assert and forget 
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
    Format.fprintf ppf "(set.singleton \"%s\")"
      s
  | s :: tl -> 
    Format.fprintf ppf "(set.union (set.singleton \"%s\") %a)" 
      s pp_print_ss tl

let pp_print_model_value ppf = function
| ConcreteBool b -> Format.pp_print_bool ppf b
| ConcreteInt i -> 
  if i >= 0 then 
    Format.pp_print_int ppf i 
  else 
    Format.fprintf ppf "(- %d)" (-1 * i) 
| ConcretePlaceholder str -> Format.pp_print_string ppf str
| ConcreteString str -> Format.fprintf ppf "\"%s\"" str 
| ConcreteStringSet ss -> pp_print_ss ppf (Utils.StringSet.to_list ss)
| ConcreteBitVector (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "#b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| ConcreteBitList bits -> 
  let print_smt_bool_seq fmt (lst : bool list) : unit =
  match lst with
  | [] ->
    Format.fprintf fmt "seq.empty"
  | [b] ->
    if b then Format.fprintf fmt "(seq.unit true)"
    else Format.fprintf fmt "(seq.unit false)"
  | _ ->
    Format.fprintf fmt "(seq.++ ";
    let print_unit b =
      if b then Format.fprintf fmt "(seq.unit true) "
      else Format.fprintf fmt "(seq.unit false) "
    in
    List.iter print_unit lst;
    Format.fprintf fmt ")"
  in print_smt_bool_seq ppf bits

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
(* label, path to this node, children *)
| Node of (string * int option) * (string * int option) list * derivation_tree list

type search_tree =
  (* DT at this node of the search tree, parent node, depth, and child nodes (if any). 
     child nodes have integer indices to denote that they are the nth expansion option. *)
| STNode of derivation_tree * search_tree option * int * (search_tree * int) list ref

let rec pp_print_derivation_tree ppf derivation_tree = match derivation_tree with 
| SymbolicLeaf _ -> Format.pp_print_string ppf "sym_leaf"
| DependentTermLeaf _ -> Format.pp_print_string ppf "dep_sym_leaf"
| ConcreteBoolLeaf (_, b) -> Format.pp_print_bool ppf b 
| ConcreteIntLeaf (_, int) -> Format.pp_print_int ppf int 
| ConcretePlaceholderLeaf (_, ph) -> Format.fprintf ppf "\"%s\"" ph 
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
| Node ((nt, Some idx), _, children) -> 
  Format.fprintf ppf "(%a%d %a)"
    Format.pp_print_string nt 
    idx
    (Lib.pp_print_list pp_print_derivation_tree " ") children
| Node ((nt, None), _, children) -> 
  Format.fprintf ppf "(%a %a)"
    Format.pp_print_string nt 
    (Lib.pp_print_list pp_print_derivation_tree " ") children

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
      let parens = parens + (List.length (String.split_on_char '(' line) - 1) 
                          - (List.length (String.split_on_char ')' line) - 1) in
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
  

let initialize_solver () : solver_instance =
  let cvc5 = Utils.find_command_in_path "cvc5" in
  let cmd = 
    Printf.sprintf "%s --produce-models --global-declarations --dag-thresh=0 --lang=smtlib2 --incremental" 
      cvc5 
  in
  let set_logic_command = Format.asprintf "(set-logic QF_BVSNIAFS)\n" in

  (*let z3 = Utils.find_command_in_path "z3" in
  let cmd = 
    Printf.sprintf "%s -smt2 -in" 
     z3 
  in
  let set_logic_command = Format.asprintf "(set-logic QF_SLIA)\n" in*)

  let (in_chan, out_chan, err_chan) = Unix.open_process_full cmd (Unix.environment ()) in
  let solver = { in_channel = in_chan; out_channel = out_chan; err_channel = err_chan } in
  issue_solver_command set_logic_command solver;
  issue_solver_command "(set-option :produce-models true)\n(set-option :global-declarations true)\n" solver;
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
  | ReStar expr -> ReStar (r expr)
  | StrToRe expr -> StrToRe (r expr) 
  | StrInRe (expr1, expr2) -> StrInRe (r expr1, r expr2) 
  | ReConcat exprs -> ReConcat (List.map r exprs)
  | ReUnion exprs -> ReUnion (List.map r exprs) 
  | ReRange (expr1, expr2) -> ReRange (r expr1, r expr2)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ 
  | EmptySet _ -> expr

let string_of_path path = 
  let path = List.map (fun (nt, idx) -> match idx with 
  | None -> nt
  | Some idx -> Format.asprintf "%s%d" nt idx
  ) path in 
  String.concat "_" path

(* Normalize a derivation tree for a fixed spot in the search tree. 
   Collect the associated constraints discovered during normalization. *)
let rec normalize_derivation_tree ctx ast declared_variables solver 
                                  constraints_to_assert dt = 
let r = normalize_derivation_tree ctx ast declared_variables solver constraints_to_assert in 
match dt with 
| Node ((nt, idx), path, []) -> 
  let forced_expansion = List.find_map (fun element -> match element with 
  | A.ProdRule (nt2, [Rhs (ges, scs)]) -> 
    let path' = (string_of_path (Utils.init path) |> String.lowercase_ascii) in
    if Utils.str_eq_ci nt nt2 then 
      let constraints_to_add = List.concat_map (fun sc -> match sc with 
      | A.SyGuSExpr e -> [universalize_expr false path e] 
      | Dependency _ -> [] 
      ) scs in
      let expr_variables = List.map A.get_nts_from_expr2 constraints_to_add |> List.flatten in
      let ty_ctx = List.fold_left (fun acc nt -> 
        let ty = Utils.StringMap.find_opt (List.rev nt |> List.hd |> fst) ctx in 
        let ty = match ty with 
        | Some ty -> ty 
        | None -> Utils.crash ("couldn't find " ^ (List.rev nt |> List.hd |> fst))
        in
        let str = Format.asprintf "%a" (Lib.pp_print_list Sygus.pp_print_nt_helper "_") nt in
        let str = 
          if path' = "" then str else path' ^ "_" ^ str 
        in 
        Utils.StringMap.add str ty acc
      ) Utils.StringMap.empty expr_variables in
      declare_smt_variables declared_variables ty_ctx solver ;
      constraints_to_assert := ConstraintSet.union !constraints_to_assert (ConstraintSet.of_list constraints_to_add); 
      let children = List.map (fun ge -> match ge with 
        | A.Nonterminal (nt, idx_opt) ->
          Node ((nt, idx_opt), path @ [nt, idx_opt], [])  
        | StubbedNonterminal (_, stub_id) -> DependentTermLeaf stub_id
        ) ges in
       Some children 
    else 
      None
  | A.ProdRule (_, _) -> None 
  | TypeAnnotation (nt2, ty, scs) ->
    let path' = string_of_path path |> String.lowercase_ascii in
    if Utils.str_eq_ci nt nt2 then 
      let constraints_to_add = List.concat_map (fun sc -> match sc with 
      | A.SyGuSExpr e -> [universalize_expr true path e] 
      | Dependency _ -> [] 
      ) scs |> ConstraintSet.of_list in
      declare_smt_variables declared_variables (Utils.StringMap.singleton path' ty) solver;
      constraints_to_assert := ConstraintSet.union !constraints_to_assert constraints_to_add;
      Some [SymbolicLeaf (ty, path @ [(nt, idx)])]
    else None 
  ) ast in 
  let children = match forced_expansion with 
  | Some children -> children 
  | None -> [] 
  in 
  Node ((nt, idx), path, List.map r children) 
| Node (nt, path, children) -> 
  Node (nt, path, List.map r children)
| leaf -> leaf 

let new_decision_level: solver_instance -> int ref -> unit 
= fun solver assertion_level ->
  let push_cmd = Format.asprintf "(push 1)" in
  assertion_level := !assertion_level + 1;
  issue_solver_command push_cmd solver; 
  ()

let rec collect_constraints_of_dt ast = function 
  | Node ((nt, _), _, children) -> 
    let child_constraints = List.map (collect_constraints_of_dt ast) children in 
    let child_constraints = List.fold_left ConstraintSet.union ConstraintSet.empty child_constraints in
    let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> nt = nt2
      ) ast in 
    let constraints = A.scs_of_element grammar_rule |> 
    List.concat_map (fun sc -> match sc with 
    | A.SyGuSExpr expr -> [expr] 
    | Dependency _ -> [] 
    ) |> ConstraintSet.of_list in 
    ConstraintSet.union constraints child_constraints
  | _ -> ConstraintSet.empty

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
    | Node (head, _, children) -> 
      if children = [] then nt_will_be_reached_ast nt head
      else (
        match List.find_opt (fun child -> match child with 
        | Node (nt2, _, _) -> str = nt2
        | SymbolicLeaf (_, path) -> str = (Utils.last path)
        | ConcreteIntLeaf (path, _) | ConcreteBoolLeaf (path, _) | ConcreteBitListLeaf (path, _) 
        | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) 
        | ConcreteStringLeaf (path, _) | ConcreteSetLeaf (path, _) -> str = (Utils.last path)
        | DependentTermLeaf nt2 -> (fst str) = nt2
        ) children with 
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

let assert_applicable_constraints constraint_set derivation_tree ast solver =
  let _constraints_to_remove = 
    ConstraintSet.fold (fun expr acc -> 
      if constraint_is_applicable expr derivation_tree ast then (
        (* declare_smt_variables declared_variables (Utils.StringMap.singleton path' A.Int) solver; *)
        (*if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is applicable in derivation tree %a\n"
          A.pp_print_expr expr 
          pp_print_derivation_tree derivation_tree;*)
        assert_smt_constraint solver expr;
        ConstraintSet.add expr acc
      ) else (
       if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is not applicable in derivation tree %a\n"
          A.pp_print_expr expr 
          pp_print_derivation_tree derivation_tree;
        acc
      )
    )  !constraint_set ConstraintSet.empty 
  in
  (* unsound! Constraints may need to be reasserted. 
     Say you have a dot notation constraint, but it doesn't become applicable until a later decision. 
     If you assert and forget, then backtrack, you may re-expand, in a different path, to the same situation 
     where that constraint holds. But, it won't be around to re-assert. And you won't re-add that constraint 
     to the constraint set because it is associated with a higher-up node. So you can only remove constraints 
     that are rooted at DT nodes that were backtracked away, not ones that are potentially rooted higher up. *)
  (*constraint_set := ConstraintSet.diff !constraint_set constraints_to_remove;*)
  !constraint_set

let initialize_globals ctx ast derivation_tree start_symbol constraints_to_assert 
                       decision_stack _declared_variables backtrack_depth curr_st_node declared_variables solver = 
  (* Keep around constraints we may not need to assert *)
  constraints_to_assert := ConstraintSet.empty; 
  (* Incremental construction of output term so far *)
  derivation_tree := (Node ((start_symbol, Some 0), [start_symbol, Some 0], []));
  derivation_tree := normalize_derivation_tree ctx ast declared_variables solver constraints_to_assert !derivation_tree ;
  constraints_to_assert := assert_applicable_constraints constraints_to_assert !derivation_tree ast solver;
  (* Set of paths through the tree to help determine when to push constraints from constraints_to_assert *)
  (* let provenance_list = _ in *) (* Challenge: backtracking affects provenance list *)
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  decision_stack := B.Stack.create ();
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  backtrack_depth := false; 
  (* Current spot in the search tree *) 
  curr_st_node := STNode (!derivation_tree, None, 0, ref []);
  ()

let sample_excluding n exclude_list =
  let excluded = List.sort_uniq compare exclude_list in
  let allowed =
    List.init n (fun i -> i + 1)
    |> List.filter (fun x -> not (List.mem x excluded))
  in
  match allowed with
  | [] -> failwith "No available values to sample"
  | _ ->
      let idx = Random.int (List.length allowed) in
      List.nth allowed idx 

(* Return expanded node, updated DT, whether or not it was a real choice *)
let find_new_expansion ast derivation_tree curr_st_node = 
  let visited_indices = match !curr_st_node with 
  | STNode (_, _, _, children) -> List.map snd !children 
  in 
  let rec num_expansions derivation_tree = match derivation_tree with 
  | Node ((nt, _), _, []) ->  
    List.find_map (function 
    | A.ProdRule (nt2, rhss) -> 
      if Utils.str_eq_ci nt nt2 then Some (List.length rhss) else None 
    | A.TypeAnnotation (nt2, _, _) ->
      if Utils.str_eq_ci nt nt2 then Some 1 else None 
    ) ast |> Option.get  
  | Node (_, _, children) -> 
    List.map num_expansions children |> 
    List.fold_left (+) 0 
  | _ -> 0 
  in 
  let rec perform_nth_expansion derivation_tree n = 
  (*if !Flags.debug then Format.fprintf Format.std_formatter "Performing %dth expansion of dt %a\n" 
    n 
    pp_print_derivation_tree derivation_tree;*)
  match derivation_tree with 
  | Node (nt, path, child :: children) -> 
    let m = num_expansions child in 
    if m >= n then 
      let expanded_node, expanded_child = perform_nth_expansion child n in 
      expanded_node, Node (nt, path, expanded_child :: children) 
    else ( 
      let expanded_node, rec_dt = perform_nth_expansion (Node (nt, path, children)) (n - m) in 
      match rec_dt with 
      | Node (nt, path, expanded_children) -> 
        expanded_node, Node (nt, path, child :: expanded_children)
      | _ -> assert false 
    )
  | Node ((nt, idx), path, []) -> 
    let element = List.find (fun element -> match element with 
    | A.TypeAnnotation (nt2, _, _) 
    | ProdRule (nt2, _) -> Utils.str_eq_ci nt nt2
    ) ast in (
    match element with 
    | TypeAnnotation (_, ty, _) -> 
      if n = 1 then 
        let expanded_node = Node ((nt, idx), path, [SymbolicLeaf (ty, path @ [(nt, idx)])]) in 
        expanded_node, expanded_node 
      else assert false 
    | ProdRule (_, rhss) -> 
      let rhs = List.nth rhss (n-1) in 
      match rhs with 
      | A.Rhs (ges, _) -> 
        let children = List.map (fun ge -> match ge with 
        | A.Nonterminal (nt, idx_opt) ->
          Node ((nt, idx_opt), path @ [nt, idx_opt], [])  
        | StubbedNonterminal (_, stub_id) -> DependentTermLeaf stub_id
        ) ges in 
        let expanded_node = Node ((nt, idx), path, children) in 
        expanded_node, expanded_node 
      | StubbedRhs str -> 
        let expanded_node = Node ((nt, idx), path, [DependentTermLeaf str]) in 
        expanded_node, expanded_node
    )
  | _ -> assert false 
  in
  let total_num_choices = num_expansions derivation_tree in 
  let index_to_pick = sample_excluding total_num_choices visited_indices in  
  let expanded_node, new_dt = perform_nth_expansion derivation_tree index_to_pick in 
  let real_choice = total_num_choices - (List.length visited_indices) > 1 in 
  expanded_node, index_to_pick, new_dt, real_choice 

let backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
              constraints_to_assert depth_limit start_symbol derivation_tree curr_st_node = 
  if !assertion_level = 1 then ( (* restarting *)
    Utils.debug_print Format.pp_print_string Format.std_formatter "Restarting...\n";
    issue_solver_command "(pop 1)" solver; 
    issue_solver_command "(push 1)" solver;
    (if not !backtrack_depth then raise (Failure "infeasible"));
    depth_limit := !depth_limit + 1;
    Format.fprintf Format.std_formatter "Increasing depth limit to %d\n" !depth_limit;
    initialize_globals ctx ast derivation_tree start_symbol constraints_to_assert 
                       decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver; 
  ) else ( 
    assertion_level := !assertion_level - 1;
    issue_solver_command "(pop 1)" solver;
    let st_node = Stack.pop !decision_stack in
    let STNode (dt, _, _, _) = st_node in 
    let original_constraints = collect_constraints_of_dt ast !derivation_tree in 
    let maintained_constraints = collect_constraints_of_dt ast dt in 
    let constraints_to_remove = ConstraintSet.diff original_constraints maintained_constraints in 
    constraints_to_assert := ConstraintSet.diff !constraints_to_assert constraints_to_remove;
    derivation_tree := dt; 
    curr_st_node := st_node
  )

let string_of_constructor (str, idx) = match idx with 
| None -> str 
| Some idx -> str ^ (string_of_int idx)

let rec model_of_sygus_ast: SygusAst.sygus_ast -> (model_value Utils.StringMap.t, unit) result
= fun sygus_ast -> 
  match sygus_ast with 
  | VarLeaf var when var = "infeasible" -> 
    Format.pp_print_flush Format.std_formatter (); Error ()
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

let get_smt_result: A.ast -> solver_instance -> bool -> (model_value Utils.StringMap.t, unit) result option
= fun ast solver get_model -> 
  issue_solver_command "(check-sat)\n" solver;
  let response = read_check_sat_response solver in
  if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
  Format.pp_print_flush Format.std_formatter ();
  if response = "sat" && get_model then (
    issue_solver_command "(get-model)\n" solver;
    let response = read_get_model_response solver in
    if !Flags.debug then Format.fprintf Format.std_formatter "Solver response: %s\n" response;
    let result = match Parsing.parse_sygus response ast with 
    | Ok result -> result 
    | Error msg -> Format.fprintf Format.std_formatter "Error parsing: %s\n" msg; assert false 
    in
    Some (model_of_sygus_ast result)
  ) else if response = "sat" then None  
  else
    let result = match Parsing.parse_sygus response ast with 
    | Ok result -> result 
    | Error msg -> Format.fprintf Format.std_formatter "Error parsing: %s\n" msg; assert false 
    in
    Some (model_of_sygus_ast result)

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
  | ConcreteBitVectorLeaf (path, _, _) | ConcretePlaceholderLeaf (path, _) 
  | ConcreteStringLeaf (path, _) | ConcreteSetLeaf (path, _) -> 
    let path' = string_of_path (Utils.init path) |> String.lowercase_ascii in
    (match Utils.StringMap.find_opt path' model with 
    | Some (ConcreteInt int) -> ConcreteIntLeaf (path, int)
    | Some (ConcreteBool bool) -> ConcreteBoolLeaf (path, bool)
    | Some (ConcreteBitList bitlist) -> ConcreteBitListLeaf (path, bitlist)
    | Some (ConcreteBitVector (len, bits)) -> ConcreteBitVectorLeaf (path, len, bits)
    | Some (ConcreteString str) -> ConcreteStringLeaf (path, str)
    | Some (ConcreteStringSet s) -> ConcreteSetLeaf (path, ConcreteStringSetLeaf s)
    | Some (ConcretePlaceholder ph) -> ConcretePlaceholderLeaf (path, ph)
    (* Model contain unconstrained variables not present in this derivation tree *)
    | None -> SymbolicLeaf (ty_of_concrete_leaf derivation_tree, path)) 
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
  | Node (nt, path, children) -> 
    let children = List.map r children in 
    Node (nt, path, children)
  
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
  | Node (nt, path, children) -> 
    let children = List.map r children in
    Node (nt, path, children)

let rec is_complete derivation_tree = match derivation_tree with
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ 
| ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ | ConcreteSetLeaf _ -> true
| Node (_, _, children) -> 
  let children = List.map is_complete children in
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
| Node (nt, _, children) -> 
  let children = List.map sygus_ast_of_derivation_tree children in 
  Node (nt, children)

(*(* Naive computation of dt_frontier *)
let rec compute_new_dt_frontier derivation_tree = match derivation_tree with 
| SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ | ConcreteSetLeaf _  
| ConcreteBoolLeaf _ | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ 
| ConcretePlaceholderLeaf _ | ConcreteStringLeaf _ -> DTSet.empty
| Node (_, _, children) as node -> 
  if children = [] then DTSet.singleton (ref node) else
  List.fold_left (fun acc child -> 
    DTSet.union acc (compute_new_dt_frontier child)
  ) DTSet.empty children *)



let pp_print_model_pair ppf (k, v) = 
  Format.fprintf ppf "(= %s %a)" 
    k 
    pp_print_model_value v 

let rec get_dt_vars = function 
| Node ((id, idx), _, children) -> 
  let id_str = Format.asprintf "%s%a"
    id (fun ppf idx -> match idx with | Some idx -> Format.pp_print_int ppf idx | None -> ()) idx in
  let r = List.map get_dt_vars children in
  let r = List.fold_left Utils.StringSet.union Utils.StringSet.empty r in
  Utils.StringSet.map (fun child -> 
    if String.equal child "" then id_str 
    else id_str ^ "_" ^ child
  ) r
| ConcreteBitVectorLeaf _| ConcreteSetLeaf _ | ConcreteBitListLeaf _ 
| ConcreteStringLeaf _ | ConcreteIntLeaf _ | ConcreteBoolLeaf _ 
| ConcretePlaceholderLeaf _ | SymbolicLeaf _ -> 
  Utils.StringSet.singleton "" 
| DependentTermLeaf _ -> Utils.StringSet.empty 

let push_blocking_clause model dt declared_variables solver = 
  let dt_vars = get_dt_vars !dt in
  let dt_vars = Utils.StringSet.map (fun s -> String.lowercase_ascii s) dt_vars in
  (*Format.fprintf Format.std_formatter "dt_vars: %a, model vars: %a\n" 
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringSet.to_list dt_vars)  
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringMap.bindings model |> List.map fst); *)
  let model = Utils.StringMap.filter (fun var _ -> Utils.StringSet.mem var dt_vars) model in
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
let dpll: A.il_type Utils.StringMap.t -> A.ast -> SA.sygus_ast
= fun ctx ast -> 
  Random.self_init (); 

  let start_symbol, start_path = match List.hd ast with 
  | A.TypeAnnotation (nt, _, _) -> nt, [nt, Some 0]
  | ProdRule (nt, _) -> nt, [nt, Some 0]
  in 

  (* Solver object *)
  let solver = initialize_solver () in
 
  (*!! IDEA: dynamically alter starting depth limit *)
  (*** HYPERPARAMETERS *)
  let starting_depth_limit = 5 in 
  let restart_rate = 10000 in 
  let num_solutions = ref 0 in 
  let num_solutions_to_find = !Flags.num_solutions in 
  let num_iterations = ref 0 in
  try

  (*** Set up the key data structures ***)
  let assertion_level = ref 0 in 
  (* Track declared (SMT-level) variables to avoid redeclaration *)
  let declared_variables = ref Utils.StringSet.empty in 
  (* Keep around constraints we may not need to assert *)
  let constraints_to_assert = ref ConstraintSet.empty in 
  (* Incremental construction of output term so far *)
  let derivation_tree = ref (Node ((start_symbol, Some 0), start_path, [])) in 
  derivation_tree := normalize_derivation_tree ctx ast declared_variables solver constraints_to_assert !derivation_tree ;
  constraints_to_assert := assert_applicable_constraints constraints_to_assert !derivation_tree ast solver;
  (* Current spot in the search tree *) 
  let curr_st_node = ref (STNode (!derivation_tree, None, 0, ref [])) in
  (* Set of paths through the tree to help determine when to push constraints from constraints_to_assert *)
  (* let provenance_list = _ in *) (* Challenge: backtracking affects provenance list *)
  (* Keep track of all decisions so we can easily backtrack in the derivation tree *)
  let decision_stack : search_tree Stack.t ref = ref (B.Stack.create ()) in 
  (* IDS depth limit *) 
  let depth_limit = ref starting_depth_limit in
  (* Track whether, since the last restart, we backtracked due to the depth limit *) 
  let backtrack_depth = ref false in 

  (* Variables for multiple-solutions flag *)
  let exit_flag = ref true in 
  let result = ref None in 

  (* we start at decision level 1 so we can undo all pushed assertions when restarting *)
  new_decision_level solver assertion_level; 
  Stack.push !curr_st_node !decision_stack;

  (* exit flag allows us to toggle between infinite looping (multiple solutions mode) 
     or stopping after one solution *)
  while !exit_flag do 
  while not (is_complete !derivation_tree) do
    Format.pp_print_flush Format.std_formatter () ;
    num_iterations := !num_iterations + 1;

    (*if !num_iterations mod 100 = 0 then 
      Format.fprintf Format.std_formatter "num_iterations: %d\n" !num_iterations; *)
     
    if !Flags.debug then Format.fprintf Format.std_formatter "------------------------\n";
    (*if !Flags.debug then Format.fprintf Format.std_formatter "Constraints to assert: %a\n"
      (Lib.pp_print_list A.pp_print_expr " ") (ConstraintSet.elements !constraints_to_assert);*)
    if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
      pp_print_derivation_tree !derivation_tree;

    (* Choose an expansion based on the search tree *)
    let expanded_node, expansion_index, new_dt, real_choice = find_new_expansion ast !derivation_tree curr_st_node in
    derivation_tree := new_dt;
    if real_choice then ( 
      new_decision_level solver assertion_level; 
      Stack.push !curr_st_node !decision_stack;
    );
    derivation_tree := normalize_derivation_tree ctx ast declared_variables solver constraints_to_assert !derivation_tree ; 
    curr_st_node := (match !curr_st_node with 
    | STNode (_, _, depth, visited) -> 
      let new_st_node = STNode (!derivation_tree, Some !curr_st_node, depth + 1, ref []) in 
      visited := (new_st_node, expansion_index) :: !visited; 
      new_st_node
    ); 

    if !Flags.debug then Format.fprintf Format.std_formatter "Expanded DT: %a\nExpanded node: %a\n" 
      pp_print_derivation_tree !derivation_tree 
      pp_print_derivation_tree expanded_node;

     if !num_iterations = restart_rate then (
        num_iterations := 0;
        (*Format.fprintf Format.std_formatter "Restarting\n";*)
        let pop_cmd = Format.asprintf "(pop %d)" !assertion_level in 
        issue_solver_command pop_cmd solver; 
        issue_solver_command "(push 1)" solver;

        (* prepare to generate another solution *)
        assertion_level := 1;
        depth_limit := starting_depth_limit;
        initialize_globals ctx ast derivation_tree start_symbol constraints_to_assert 
                           decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver; 
    ) else 

    (* Assert constraints for the expanded node *)
    match expanded_node, !curr_st_node with 
    (SymbolicLeaf _ | ConcreteIntLeaf _ | DependentTermLeaf _ | ConcreteSetLeaf _ 
    | ConcreteBitListLeaf _ | ConcreteBitVectorLeaf _ | ConcreteBoolLeaf _ 
    | ConcretePlaceholderLeaf _ | ConcreteStringLeaf _), _  -> () (* nothing else to do *)
    | Node (_, _, []), STNode _ -> assert false
    | Node (nt, path, children), STNode (_, _, depth, _) -> 
      if !Flags.debug then Format.fprintf Format.std_formatter "Current search tree depth: %d\n" 
        depth;

      (* Backtrack due to depth limit if necessary *)
      if depth > !depth_limit then (
        backtrack_depth := true;
        Utils.debug_print Format.pp_print_string Format.std_formatter 
          ("Exceeded depth limit " ^ (string_of_int !depth_limit) ^ "!\n");
        backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  constraints_to_assert depth_limit start_symbol derivation_tree curr_st_node 
      ) else 

      (* Find the associated AST rule for the new expansion *)
      let grammar_rule = List.find (fun element -> match element with 
      | A.ProdRule (nt2, _) 
      | TypeAnnotation (nt2, _, _) -> Utils.str_eq_ci (fst nt) nt2
      ) ast in 
      let path' = string_of_path path |> String.lowercase_ascii in

      (* Assert semantic constraints for the new expansion (backtrack if necessary) *)
      match grammar_rule with 
      | A.TypeAnnotation (_, _, []) -> ()
      | A.TypeAnnotation (_, ty, scs) -> 
        List.iter (fun sc -> match sc with 
        | A.SyGuSExpr expr ->
          declare_smt_variables declared_variables (Utils.StringMap.singleton path' ty) solver; 
          constraints_to_assert := ConstraintSet.add (universalize_expr true path expr) !constraints_to_assert;
          constraints_to_assert := assert_applicable_constraints constraints_to_assert !derivation_tree ast solver;
          let model = get_smt_result ast solver false in  
          (match model with 
          | Some (Ok _) -> assert false
          | None -> 
            if !Flags.debug then Format.pp_print_string Format.std_formatter 
              "it was SAT, waiting to expand before instantiating in derivation tree\n"; 
          | Some (Error ()) -> 
            backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  constraints_to_assert depth_limit start_symbol derivation_tree curr_st_node
          )
        | A.Dependency _ -> ()
        ) scs;
      | A.ProdRule (_, rhss) -> 
        (*Format.fprintf Format.std_formatter "Finding the chosen rule for %a\n" 
          pp_print_derivation_tree expanded_node; *)
        let chosen_rule = List.find (fun rhs -> match rhs with 
        | A.StubbedRhs str1 -> (
          match children with 
          | [DependentTermLeaf str2] -> Utils.str_eq_ci str1 str2 
          | _ -> false 
          )
        | Rhs (ges, _) -> 
          if List.length ges = List.length children then 
            List.for_all2 (fun child ge -> match child, ge with 
            | Node ((nt, idx), _, _), A.Nonterminal (nt2, idx2) -> 
              Utils.str_eq_ci nt nt2 && idx = idx2
            | DependentTermLeaf stub_id1, A.StubbedNonterminal (_, stub_id2) -> 
              Utils.str_eq_ci stub_id1 stub_id2
            | _ -> false 
            ) children ges 
          else false 
        ) rhss in
        if !Flags.debug then Format.fprintf Format.std_formatter "Chose rule %a\n" 
          A.pp_print_prod_rule_rhs chosen_rule;
        match chosen_rule with 
        | A.StubbedRhs _ -> () 
        | A.Rhs (_, scs) -> 
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
            (* derivation_tree := instantiate_terminals model derivation_tree;  *)
          | A.Dependency _ -> ()
          ) scs;

          (* Assert the constraints from this choice (and also try to assert constraints hanging around from earlier on,
             but maybe weren't definitely applicable until this decision) *)
          constraints_to_assert := assert_applicable_constraints constraints_to_assert !derivation_tree ast solver;
          let model = get_smt_result ast solver false in
          (match model with 
          | None -> (* sat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter 
              "it was SAT, waiting to expand before instantiating in derivation tree\n"; 
          | Some (Error ()) -> (* unsat *)
            if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
            backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  constraints_to_assert depth_limit start_symbol derivation_tree curr_st_node 
          | Some _ -> assert false
          ); 

  done;

  if !Flags.debug then Format.fprintf Format.std_formatter "Constraints to assert: %a\n"
    (Lib.pp_print_list A.pp_print_expr " ") (ConstraintSet.elements !constraints_to_assert);
  if !Flags.debug then Format.fprintf Format.std_formatter "Derivation tree: %a\n"
    pp_print_derivation_tree !derivation_tree;

  (* Handle any remaining constraints from last iteration of the loop *)
  let model = get_smt_result ast solver true in
  (match model with 
  | Some (Ok model) -> (* sat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was SAT, instantiating in derivation tree\n"; 
    derivation_tree := instantiate_terminals model !derivation_tree; 
  | None -> assert false
  | Some (Error ()) -> (* unsat *)
    if !Flags.debug then Format.pp_print_string Format.std_formatter "it was UNSAT, backtracking\n"; 
    backtrack ctx ast assertion_level decision_stack solver backtrack_depth declared_variables 
                  constraints_to_assert depth_limit start_symbol derivation_tree curr_st_node 
  ); 

  (* If we exited the loop, there must be a model. If there was no model, 
     we would have backtracked, and the derivation tree would still be open, 
     so the loop would continue. *)
  let model = match model with 
  | Some (Ok model) -> model 
  | _ -> Utils.crash "internal error; expected a model but got none." in 

  derivation_tree := fill_unconstrained_nonterminals !derivation_tree;
  (* Convert to sygus AST for later processing in the pipeline *)
  let r = sygus_ast_of_derivation_tree !derivation_tree in 
  result := Some r;
  exit_flag := false;
  if !Flags.multiple_solutions then (
    num_iterations := 0;
    num_solutions := !num_solutions + 1;
    exit_flag := not (!num_solutions = num_solutions_to_find);
    Format.fprintf Format.std_formatter "%a$\n" 
      SA.pp_print_sygus_ast r;
    Format.pp_print_flush Format.std_formatter ();

    (* Need to pop all the way back to zeroth level so we can assert persisting blocking clause *)
    let pop_cmd = Format.asprintf "(pop %d)" !assertion_level in 
    issue_solver_command pop_cmd solver; 
    if !Flags.debug then Format.fprintf Format.std_formatter "Pushing blocking clause\n" ;
    push_blocking_clause model derivation_tree declared_variables solver;
    issue_solver_command "(push 1)" solver;

    (* prepare to generate another solution *)
    assertion_level := 1;
    depth_limit := starting_depth_limit;
    initialize_globals ctx ast derivation_tree start_symbol constraints_to_assert 
                       decision_stack declared_variables backtrack_depth curr_st_node declared_variables solver; 
  );
  ()
  done; 

  Option.get !result 

  with Failure e -> 
    (if not (String.equal e "infeasible") then 
      Utils.crash e);
    Format.pp_print_flush Format.std_formatter ();
    StrLeaf "infeasible"
