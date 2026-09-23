open Ast

let rec calculate_casts: expr -> expr 
= fun expr -> match expr with 
| BVCast (len, expr, p) -> (
  match expr with 
  | IntConst (i, p) -> Ast.il_int_to_bv len i p
  | _ -> BVCast (len, expr, p)
  )
| BinOp (expr1, op, expr2, p) -> BinOp (calculate_casts expr1, op, calculate_casts expr2, p) 
| ActLit (nt_expr, p) -> ActLit (calculate_casts nt_expr, p)
| UnOp (op, expr, p) -> UnOp (op, calculate_casts expr, p) 
| Singleton (expr, p) -> Singleton (calculate_casts expr, p)
| CompOp (expr1, op, expr2, p) -> CompOp (calculate_casts expr1, op, calculate_casts expr2, p) 
| BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map calculate_casts exprs, p)
| NTExpr _ 
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

let stub_grammar_element: semantic_constraint list -> grammar_element -> semantic_constraint option * grammar_element
= fun scs ge -> match ge with 
| StubbedNonterminal _ -> None, ge 
| Nonterminal (nt, _, _, _, _) -> (
  match List.find_opt (fun sc -> match sc with
  | SmtConstraint _ -> false 
  | DerivedField (nt2, _, _) -> Nt.equal nt nt2
  | AttrDef _ -> assert false
  ) scs with 
  | Some dep -> 
    let stub = Nt.fresh_stub nt in
    Some dep, StubbedNonterminal stub
  | None -> None, ge
  )

let stub_ty_annot
= fun nt ty scs p -> 
  match List.find_opt (fun sc -> match sc with
  | SmtConstraint _ -> false 
  | DerivedField (nt2, _, _) -> Nt.equal nt nt2
  | AttrDef _ -> assert false
  ) scs with 
  | Some dep -> 
    let stub = Nt.fresh_stub nt in
    Nt.StubMap.singleton stub dep, ProdRule (nt, [], [Rhs ([StubbedNonterminal stub], [], None, p)], p)
  | None -> Nt.StubMap.empty, TypeAnnotation (nt, ty, scs, p)


let simp_rhss: prod_rule_rhs -> semantic_constraint Nt.StubMap.t * prod_rule_rhs 
= fun rhss -> match rhss with 
| Rhs (ges, scs, prob, p) ->
  let scs = List.map (fun sc -> match sc with 
  | DerivedField (nt, expr, p) -> DerivedField (nt, calculate_casts expr, p)
  | SmtConstraint (expr, p) -> SmtConstraint (calculate_casts expr, p)
  | AttrDef _ -> assert false
  ) scs in 
  (* Abstract away dependent terms. Whenever we abstract away a term, we store 
     a mapping from the abstracted stub ID to the original dependency *)
  let dep_map, ges = List.fold_left (fun (acc_dep_map, acc_ges) ge -> 
    match stub_grammar_element scs ge with 
    | Some dep, StubbedNonterminal stub -> 
      Nt.StubMap.add stub dep acc_dep_map, 
      acc_ges @ [StubbedNonterminal stub]
    | None, ge -> acc_dep_map, acc_ges @ [ge]
    | Some _, _ -> assert false 
  ) (Nt.StubMap.empty, []) ges in 
  dep_map, Rhs (ges, scs, prob, p)
| StubbedRhs _ as rhs -> Nt.StubMap.empty, rhs 


(*     let dep_map = List.fold_left (Nt.StubMap.merge Lib.union_keys) acc_dep_map dep_maps in *)

let simp_ast: ast -> (semantic_constraint Nt.StubMap.t * ast) 
= fun ast -> 
  let dep_map, ast = List.fold_left (fun (acc_dep_map, acc_elements) element -> match element with 
  | ProdRule (nt, ias, rhss, p) -> 
    let dep_map, rhss = List.fold_left (fun (acc_dep_map, acc_rhss) rhs -> 
      let dep_map, rhs = simp_rhss rhs in 
      let dep_map = Nt.StubMap.merge Lib.union_keys dep_map acc_dep_map in
      dep_map, rhs :: acc_rhss
    ) (acc_dep_map, []) rhss in
    let dep_map = Nt.StubMap.merge Lib.union_keys dep_map Nt.StubMap.empty in
    dep_map, ProdRule (nt, ias, List.rev rhss, p) :: acc_elements 
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (fun sc -> match sc with 
    | DerivedField (nt, expr, p) -> DerivedField (nt, calculate_casts expr, p)
    | SmtConstraint (expr, p) -> SmtConstraint (calculate_casts expr, p)
    | AttrDef _ -> assert false
    ) scs in 
    let dep_map, element = stub_ty_annot nt ty scs p in
    let dep_map = Nt.StubMap.merge Lib.union_keys dep_map acc_dep_map in
    dep_map, element :: acc_elements
  ) (Nt.StubMap.empty, []) ast  in 
  dep_map, List.rev ast

let abstract_dependencies: ast -> (semantic_constraint Nt.StubMap.t * ast)  
= fun ast -> simp_ast ast
