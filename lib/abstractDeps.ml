open Ast

let rec calculate_casts: expr -> expr 
= fun expr -> match expr with 
| BVCast (len, expr, p) -> (
  match expr with 
  | IntConst (i, p) -> Ast.il_int_to_bv len i p
  | _ -> BVCast (len, expr, p)
  )
| BinOp (expr1, op, expr2, p) -> BinOp (calculate_casts expr1, op, calculate_casts expr2, p) 
| UnOp (op, expr, p) -> UnOp (op, calculate_casts expr, p) 
| Singleton (expr, p) -> Singleton (calculate_casts expr, p)
| CompOp (expr1, op, expr2, p) -> CompOp (calculate_casts expr1, op, calculate_casts expr2, p) 
| Match (nt_ctx, nt, cases, p) -> 
  let cases = List.map (fun case -> match case with 
  | CaseStub _ -> case 
  | Case (nts, e) -> Case (nts, calculate_casts e)
  ) cases in
  Match (nt_ctx, nt, cases, p)
| BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map calculate_casts exprs, p)
| NTExpr _ 
| BVConst _ 
| BLConst _ 
| BConst _ 
| IntConst _ 
| PhConst _ 
| StrConst _ 
| EmptySet _ -> expr

let stub_grammar_element: TypeChecker.context -> semantic_constraint list -> grammar_element -> semantic_constraint option * grammar_element * TypeChecker.context
= fun ctx scs ge -> match ge with 
| StubbedNonterminal _ -> None, ge, ctx 
| Nonterminal (nt, _, _) -> (
  match List.find_opt (fun sc -> match sc with
  | SmtConstraint _ -> false 
  | DerivedField (nt2, _, _) -> nt = nt2
  ) scs with 
  | Some dep -> 
    let stub_id = Utils.mk_fresh_stub_id nt in
    let ctx = Utils.StringMap.remove nt ctx in
    Some dep, StubbedNonterminal (nt, stub_id), ctx  
  | None -> None, ge, ctx
  )

let stub_ty_annot
= fun ctx nt ty scs p -> 
  match List.find_opt (fun sc -> match sc with
  | SmtConstraint _ -> false 
  | DerivedField (nt2, _, _) -> nt = nt2
  ) scs with 
  | Some dep -> 
    let stub_id = Utils.mk_fresh_stub_id nt in
    let ctx = Utils.StringMap.remove nt ctx in
    Utils.StringMap.singleton stub_id dep, ProdRule (nt, [Rhs ([StubbedNonterminal(nt, stub_id)], [], None, p)], p), ctx
  | None -> Utils.StringMap.empty, TypeAnnotation (nt, ty, scs, p), ctx


let simp_rhss: TypeChecker.context -> prod_rule_rhs -> semantic_constraint Utils.StringMap.t * prod_rule_rhs * TypeChecker.context 
= fun ctx rhss -> match rhss with 
| Rhs (ges, scs, prob, p) ->
  let scs = List.map (fun sc -> match sc with 
  | DerivedField (nt, expr, p) -> DerivedField (nt, calculate_casts expr, p)
  | SmtConstraint (expr, p) -> SmtConstraint (calculate_casts expr, p)
  ) scs in 
  (* Abstract away dependent terms. Whenever we abstract away a term, we store 
     a mapping from the abstracted stub ID to the original dependency *)
  let dep_map, ges, ctx = List.fold_left (fun (acc_dep_map, acc_ges, acc_ctx) ge -> 
    match stub_grammar_element acc_ctx scs ge with 
    | Some dep, StubbedNonterminal (nt, stub_id), ctx -> 
      Utils.StringMap.add (String.uppercase_ascii stub_id) dep acc_dep_map, 
      acc_ges @ [StubbedNonterminal (nt, stub_id)], 
      ctx
    | None, ge, ctx -> acc_dep_map, acc_ges @ [ge], ctx
    | Some _, _, _ -> assert false 
  ) (Utils.StringMap.empty, [], ctx) ges in 
  dep_map, Rhs (ges, scs, prob, p), ctx
| StubbedRhs _ as rhs -> Utils.StringMap.empty, rhs, ctx 


(*     let dep_map = List.fold_left (Utils.StringMap.merge Lib.union_keys) acc_dep_map dep_maps in *)

let simp_ast: TypeChecker.context -> ast -> (semantic_constraint Utils.StringMap.t * ast * TypeChecker.context) 
= fun ctx ast -> 
  let dep_map, ast, ctx = List.fold_left (fun (acc_dep_map, acc_elements, acc_ctx) element -> match element with 
  | ProdRule (nt, rhss, p) -> 
    let dep_map, rhss, ctx = List.fold_left (fun (acc_dep_map, acc_rhss, acc_ctx) rhs -> 
      let dep_map, rhs, ctx = (simp_rhss acc_ctx rhs) in 
      let dep_map = Utils.StringMap.merge Lib.union_keys dep_map acc_dep_map in
      dep_map, rhs :: acc_rhss, ctx
    ) (acc_dep_map, [], acc_ctx)  rhss in
    let dep_map = Utils.StringMap.merge Lib.union_keys dep_map Utils.StringMap.empty in
    dep_map, ProdRule (nt, List.rev rhss, p) :: acc_elements, ctx 
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (fun sc -> match sc with 
    | DerivedField (nt, expr, p) -> DerivedField (nt, calculate_casts expr, p)
    | SmtConstraint (expr, p) -> SmtConstraint (calculate_casts expr, p)
    ) scs in 
    let dep_map, element, ctx = stub_ty_annot acc_ctx nt ty scs p in
    let dep_map = Utils.StringMap.merge Lib.union_keys dep_map acc_dep_map in
    dep_map, element :: acc_elements, ctx
  ) (Utils.StringMap.empty, [], ctx) ast  in 
  dep_map, List.rev ast, ctx

let abstract_dependencies: TypeChecker.context -> ast -> (semantic_constraint Utils.StringMap.t * ast * TypeChecker.context)  
= fun ctx ast -> simp_ast ctx ast
