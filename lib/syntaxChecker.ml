(*
  1. No dangling identifiers (no identifiers in RHS of some prod rule or in 
     semantic constraint that aren't on the LHS of some prod rule or have type annotation)
  2. All nonterminal expression dot notation chains are valid 
      a) NTExpr starts with a nonterminal on rule RHS if prod rule or rule LHS if type annotation 
      b) Each dot references a valid nonterminal
  3. No nonterminal has both a production rule and a type annotation

  (* TODO: For now, don't support redundant rule options (same list of NTs). 
       If we allow this, then match statements are a bit harder to generate, 
       because you could accidentally match the same constructor twice 
       rather than the distinct constructors. *)
*)

open Ast

module StringSet = Set.Make(
  struct type t = string 
  let compare = Stdlib.compare 
  end
)

type prod_rule_map = (Utils.StringSet.t) Utils.StringMap.t

(* Build production rule map, which is a map from each grammar nonterminal 
   to the list of valid nonterminal references *)
let build_prm: ast -> prod_rule_map
= fun ast -> 
  let prm = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, rhss) -> 
    List.fold_left (fun acc rhss -> match rhss with 
    | Rhs (ges, _) -> 
      let grammar_elements = List.map Ast.grammar_element_to_string ges in
      let grammar_elements = Utils.StringSet.of_list grammar_elements in (
      match Utils.StringMap.find_opt nt acc with 
      | Some mem -> 
        if (Utils.StringSet.is_empty mem) then 
          Utils.error ("Nonterminal " ^ nt ^ " has both a type annotation and a production rule")
        else Utils.StringMap.add nt (Utils.StringSet.union mem grammar_elements) acc 
      | None -> 
        Utils.StringMap.add nt grammar_elements acc     
      )
    | StubbedRhs _ -> acc
    ) acc rhss
  | TypeAnnotation (nt, _, _) -> 
    match Utils.StringMap.find_opt nt acc with 
    | Some _ -> 
      Utils.error ("Nonterminal " ^ nt ^ " either has two type annotations, or has both a type annotation and a production rule")
    | None -> 
      Utils.StringMap.add nt Utils.StringSet.empty acc 
  ) Utils.StringMap.empty ast in 
  prm

(* Build nonterminal set, which the set of nonterminals with either
   their own production rules or type annotations *)
let build_nt_set: ast -> Utils.StringSet.t 
= fun ast -> 
  List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, _)
  | TypeAnnotation (nt, _, _) -> Utils.StringSet.add nt acc
  ) Utils.StringSet.empty ast

let rec check_dangling_identifiers: Utils.StringSet.t -> expr -> expr 
= fun nt_set expr -> 
  let call = check_dangling_identifiers nt_set in 
  let check_d_ids_nt_expr nt_expr = 
    List.iter (fun nt -> match Utils.StringSet.find_opt nt nt_set with 
    | None -> Utils.error ("Dangling identifier " ^ nt)
    | Some _ -> ()
    ) nt_expr
  in
  match expr with 
  | NTExpr (nt_ctx, nt_expr) -> 
    let nt_expr' = List.map fst nt_expr in
    let _ = check_d_ids_nt_expr nt_expr' in 
    NTExpr (nt_ctx, nt_expr)
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | StrLength expr -> StrLength (call expr)
  | Length expr -> Length (call expr) 
  | Match _ -> assert false (* -> Match (check_d_ids_nt_expr nt_expr, cases) *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

let rec check_nt_expr_refs: prod_rule_map -> (string * int option) list -> (string * int option) list 
= fun prm nt_expr -> match nt_expr with 
| (nt1, idx1) :: (nt2, idx2) :: tl ->
  if (not (Utils.StringSet.mem nt2 (Utils.StringMap.find nt1 prm))) 
  then 
    let sub_expr_str = Utils.capture_output Ast.pp_print_nt_with_dots [(nt1, idx1); (nt2, idx2)] in
    Utils.error ("Dot notation " ^ sub_expr_str ^ " is an invalid reference" )
  else (nt1, idx1) :: check_nt_expr_refs prm ((nt2, idx2) :: tl)
| _ -> nt_expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_prod_rule_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_prod_rule_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_context, nt_expr) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr |> fst) nts)) 
    then Utils.error ("Nonterminal " ^  (List.hd nt_expr |> fst) ^ " not found in current production rule or type annotation")
    else
      let nt_expr = check_nt_expr_refs prm nt_expr in 
      NTExpr (nt_context, nt_expr) 
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | StrLength expr -> StrLength (call expr)
  | Length expr -> Length (call expr) 
  | Match _ -> assert false (* -> Match (check_nt_expr_refs prm nt_expr, cases) *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_type_annot_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_type_annot_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_context, nt_expr) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr |> fst) nts)) 
    then Utils.error ("Nonterminal " ^  (List.hd nt_expr |> fst) ^ " not found in current production rule or type annotation")
    else
      let nt_expr = check_nt_expr_refs prm nt_expr in 
      NTExpr (nt_context, nt_expr) 
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | StrLength expr -> StrLength (call expr)
  | Length expr -> Length (call expr) 
  | Match _ -> assert false(* -> Match (check_nt_expr_refs prm nt_expr, cases) *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

let check_syntax_prod_rule: prod_rule_map -> Utils.StringSet.t -> prod_rule_rhs -> prod_rule_rhs
= fun prm nt_set rhss -> match rhss with 
| Rhs (ges, scs) ->
  let ges' = List.map Ast.grammar_element_to_string ges in
  let scs = List.map (fun sc -> match sc with 
  | Dependency (nt2, expr) -> 
    if (not (Utils.StringSet.mem nt2 nt_set)) then Utils.error ("Dangling identifier " ^ nt2) else
    if (not (List.mem nt2 ges')) then Utils.error ("Dependency LHS identifier " ^ nt2 ^ " is not present on the RHS of the corresponding production rule") else
    let expr = check_dangling_identifiers nt_set expr in 
    let expr = check_prod_rule_nt_exprs prm (Utils.StringSet.of_list ges') expr in
    Dependency (nt2, expr)
  | SyGuSExpr expr -> 
    let expr = check_dangling_identifiers nt_set expr in 
    let expr = check_prod_rule_nt_exprs prm (Utils.StringSet.of_list ges') expr in
    SyGuSExpr expr
  ) scs in 
  Rhs (ges, scs)
| StubbedRhs _ -> assert false

let rhss_contains_nt nt rhss = 
  List.iter (fun rhs -> match rhs with 
  | Rhs (ges, _) -> List.iter (fun ge -> match ge with 
    | Nonterminal nt2
    | StubbedNonterminal (nt2, _) -> 
      if nt = nt2 then 
        Utils.warning_print Format.pp_print_string Format.std_formatter "Warning: Grammar is recursive. Constraints on recursive production rules may not be respected.\n"
      else ()
  ) ges
  | StubbedRhs _ -> ()
  ) rhss

(* let check_if_recursive: ast -> ast 
= fun ast -> 
  match TopologicalSort.canonicalize ast with 
  | Some ast -> List.map (fun element -> match element with
    | ProdRule (nt, rhss) -> 
      let _ = rhss_contains_nt nt rhss in
      ProdRule (nt, rhss)
    | _ -> element
  ) ast 
  | None -> Utils.error "Recursive grammars are not supported" *)

let check_vacuity: ast -> ast 
= fun ast -> 
  if ast = [] then Utils.error "Grammar is empty after dead rule removal"
  else ast

let remove_circular_deps: ast -> ast 
= fun ast -> 
  List.map (fun element -> match element with
    | TypeAnnotation _ -> element 
    | ProdRule (nt, rhss) -> let rhss = List.map (fun rhs -> match rhs with
        | StubbedRhs _ -> rhs 
        | Rhs (nt, scs) -> 
          let sygus_exprs = List.filter (fun sc -> match sc with
          | SyGuSExpr _ -> true 
          | Dependency _ -> false
          ) scs in 
          let dependencies = List.filter (fun sc -> match sc with
          | SyGuSExpr _ -> false 
          | Dependency _ -> true
          ) scs in
          let dependencies = match TopologicalSort.canonicalize_scs dependencies with 
          | None -> dependencies
          | Some cycle -> 
            List.fold_left (fun acc dep -> match dep with 
            | SyGuSExpr _ -> dep :: acc  
            | Dependency (nt, expr) -> 
              (* Change dependent terms in cycle to sygus level constraints *)
              if List.mem nt cycle
              then (
                Utils.debug_print Format.pp_print_string Format.std_formatter "Replacing dependency with SyGuS constraint\n";
                SyGuSExpr (CompOp (NTExpr ([], [nt, None]), Eq, expr)) :: acc
              ) else dep :: acc
            ) [] dependencies
          in 
          Rhs (nt, sygus_exprs @ dependencies)
      ) rhss in 
      ProdRule (nt, rhss)
  ) ast

let check_scs_for_dep_terms: semantic_constraint list -> semantic_constraint list  
= fun scs -> 
  let dep_terms = List.fold_left (fun acc sc -> match sc with 
  | Dependency (nt, _) -> StringSet.add nt acc 
  | _ -> acc
  ) StringSet.empty scs in
  let deps_to_convert = List.fold_left (fun acc sc -> match sc with 
  | Dependency _ -> acc
  | SyGuSExpr expr -> 
    let nts = Ast.get_nts_from_expr expr |> StringSet.of_list in 
    let intersection = StringSet.inter dep_terms nts in
    if StringSet.is_empty intersection then acc
    else 
      let deps_to_convert = intersection in
      StringSet.union acc deps_to_convert
  ) StringSet.empty scs in 
  List.fold_left (fun acc sc -> match sc with 
  | Dependency (nt, expr) -> 
    if StringSet.mem nt deps_to_convert then (
      Utils.debug_print Format.pp_print_string Format.std_formatter "Replacing dependency with SyGuS constraint\n";
      SyGuSExpr (CompOp (NTExpr ([], [nt, None]), Eq, expr)) :: acc
    ) else sc :: acc
  | SyGuSExpr _ -> sc :: acc
  ) [] scs |> List.rev

let check_sygus_exprs_for_dep_terms: ast -> ast 
= fun ast -> 
  List.map (fun element -> match element with 
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = check_scs_for_dep_terms scs in 
    TypeAnnotation (nt, ty, scs)
  | ProdRule (nt, rhss) -> 
    let rhss = List.map (fun rhs -> match rhs with
    | Rhs (ges, scs) -> Rhs (ges, check_scs_for_dep_terms scs)
    | StubbedRhs _ -> rhs
    ) rhss in 
    ProdRule (nt, rhss)
  ) ast

let check_syntax: prod_rule_map -> Utils.StringSet.t -> ast -> ast 
= fun prm nt_set ast -> 
  (* let ast = check_if_recursive ast in *)
  let ast = Utils.recurse_until_fixpoint ast (=) remove_circular_deps in
  let ast = Utils.recurse_until_fixpoint ast (=) check_sygus_exprs_for_dep_terms in
  let ast = check_vacuity ast in
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, rhss) -> 
    let rhss = List.map (check_syntax_prod_rule prm nt_set) rhss in
    ProdRule (nt, rhss)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) ->
      if (not (Utils.StringSet.mem nt2 nt_set)) then Utils.error ("Dangling identifier " ^ nt2) else
      if (not (nt2 = nt)) then Utils.error ("Dependency LHS identifier " ^ nt2 ^ " is not present in the corresponding type annotation") else
      let expr = check_dangling_identifiers nt_set expr in 
      let expr = check_type_annot_nt_exprs prm (Utils.StringSet.singleton nt) expr in
      Dependency (nt2, expr) 
    | SyGuSExpr expr -> 
      let expr = check_dangling_identifiers nt_set expr in  
      let expr = check_type_annot_nt_exprs prm (Utils.StringSet.singleton nt) expr in
      SyGuSExpr expr
    ) scs in 
    TypeAnnotation (nt, ty, scs)
  ) ast in 
  ast