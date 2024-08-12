(*
  1. No dangling identifiers (no identifiers in RHS of some prod rule or in 
     semantic constraint that aren't on the LHS of some prod rule or have type annotation)
  2. All nonterminal expression dot notation chains are valid 
      a) NTExpr starts with a nonterminal on rule RHS if prod rule or rule LHS if type annotation 
      b) Each dot references a valid nonterminal
  3. No nonterminal has both a production rule and a type annotation
*)

open Ast

type prod_rule_map = (Utils.StringSet.t) Utils.StringMap.t

(* Build production rule map, which is a map from each grammar nonterminal 
   to the list of valid nonterminal references *)
let build_prm: ast -> prod_rule_map
= fun ast -> 
  let prm = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, rhss) -> 
    List.fold_left (fun acc rhss -> match rhss with 
    | Rhs (ges, _) -> 
      let grammar_elements = List.map Utils.grammar_element_to_string ges in
      let grammar_elements = Utils.StringSet.of_list grammar_elements in (
      match Utils.StringMap.find_opt nt acc with 
      | Some mem -> 
        if (Utils.StringSet.is_empty mem) then 
          failwith ("Nonterminal " ^ nt ^ " has both a type annotation and a production rule")
        else Utils.StringMap.add nt (Utils.StringSet.union mem grammar_elements) acc 
      | None -> 
        Utils.StringMap.add nt grammar_elements acc     
      )
    | StubbedRhs _ -> acc
    ) acc rhss
  | TypeAnnotation (nt, _, _) -> 
    match Utils.StringMap.find_opt nt acc with 
    | Some _ -> 
      failwith ("Nonterminal " ^ nt ^ " either has two type annotations, or has both a type annotation and a production rule")
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
    List.map (fun nt -> match Utils.StringSet.find_opt nt nt_set with 
    | None -> failwith ("Dangling identifier " ^ nt)
    | Some _ -> nt
    ) nt_expr
  in
  match expr with 
  | NTExpr (nt_expr, index) -> 
    let nt_expr = check_d_ids_nt_expr nt_expr in 
    NTExpr (nt_expr, index) 
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | Length expr -> Length (call expr) 
  | CaseExpr (nt_expr, cases) -> CaseExpr (check_d_ids_nt_expr nt_expr, cases) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | StrConst _ -> expr

let rec check_nt_expr_refs: prod_rule_map -> nt_expr -> nt_expr 
= fun prm nt_expr -> match nt_expr with 
| nt1 :: nt2 :: tl ->
  if (not (Utils.StringSet.mem nt2 (Utils.StringMap.find nt1 prm))) 
  then 
    let sub_expr_str = Utils.capture_output Ast.pp_print_nt_expr [nt1; nt2] in
    failwith ("Dot notation " ^ sub_expr_str ^ " is an invalid reference" )
  else nt1 :: check_nt_expr_refs prm (nt2 :: tl)
| _ -> nt_expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_prod_rule_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_prod_rule_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_expr, index) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr) nts)) 
    then failwith ("Nonterminal " ^  (List.hd nt_expr) ^ " not found in current production rule or type annotation")
    else
      let nt_expr = check_nt_expr_refs prm nt_expr in 
      NTExpr (nt_expr, index) 
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | Length expr -> Length (call expr) 
  | CaseExpr (nt_expr, cases) -> CaseExpr (check_nt_expr_refs prm nt_expr, cases) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | StrConst _ -> expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_type_annot_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_type_annot_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_expr, index) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr) nts)) 
    then failwith ("Nonterminal " ^  (List.hd nt_expr) ^ " not found in current production rule or type annotation")
    else
      let nt_expr = check_nt_expr_refs prm nt_expr in 
      NTExpr (nt_expr, index) 
  | BinOp (expr1, op, expr2) -> BinOp (call expr1, op, call expr2) 
  | UnOp (op, expr) -> UnOp (op, call expr) 
  | CompOp (expr1, op, expr2) -> CompOp (call expr1, op, call expr2) 
  | Length expr -> Length (call expr) 
  | CaseExpr (nt_expr, cases) -> CaseExpr (check_nt_expr_refs prm nt_expr, cases) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ 
  | StrConst _ -> expr

let check_syntax_prod_rule: prod_rule_map -> Utils.StringSet.t -> prod_rule_rhs -> prod_rule_rhs
= fun prm nt_set rhss -> match rhss with 
| Rhs (ges, scs) ->
  let ges' = List.map Utils.grammar_element_to_string ges in
  let scs = List.map (fun sc -> match sc with 
  | Dependency (nt2, expr) -> 
    if (not (Utils.StringSet.mem nt2 nt_set)) then failwith ("Dangling identifier " ^ nt2) else
    if (not (List.mem nt2 ges')) then failwith ("Dependency LHS identifier " ^ nt2 ^ " is not present on the RHS of the corresponding production rule") else
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

let check_syntax: prod_rule_map -> Utils.StringSet.t -> ast -> ast 
= fun prm nt_set ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, rhss) -> 
    let rhss = List.map (check_syntax_prod_rule prm nt_set) rhss in
    ProdRule (nt, rhss)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) ->
      if (not (Utils.StringSet.mem nt2 nt_set)) then failwith ("Dangling identifier " ^ nt2) else
      if (not (nt2 = nt)) then failwith ("Dependency LHS identifier " ^ nt2 ^ " is not present in the corresponding type annotation") else
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