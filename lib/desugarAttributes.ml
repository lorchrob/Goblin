module A = Ast

let rec synth_attr_to_nt_expr 
= fun expr -> 
  let r = synth_attr_to_nt_expr in
  match expr with
  | A.SynthAttr (nt, attr, p) ->
    let nt1 = nt, None in 
    let nt2 = "_" ^ attr, None in 
    A.NTExpr ([], [nt1; nt2], p)
  | A.BVCast (len, expr, pos) -> A.BVCast (len, r expr, pos)
  | BinOp (expr1, op, expr2, pos) -> BinOp (r expr1, op, r expr2, pos) 
  | UnOp (op, expr, pos) -> UnOp (op, r expr, pos) 
  | CompOp (expr1, op, expr2, pos) -> CompOp (r expr1, op, r expr2, pos) 
  | Singleton (expr, pos) -> Singleton (r expr, pos)
  | BuiltInFunc (func, exprs, pos) -> BuiltInFunc (func, List.map r exprs, pos) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | NTExpr _
  | StrConst _
  | EmptySet _ -> expr
  | Match _ -> assert false

let handle_sc _ctx sc = match sc with 
| A.DerivedField (nt, expr, p) -> 
  let expr = synth_attr_to_nt_expr expr in 
  A.DerivedField (nt, expr, p), None
| A.SmtConstraint (expr, p) -> 
  let expr = synth_attr_to_nt_expr expr in 
  A.SmtConstraint (expr, p), None
| A.AttrDef (attr, expr, p) ->
  let expr = synth_attr_to_nt_expr expr in 
  let c = A.CompOp (NTExpr ([], ["_" ^ attr, None], p), A.Eq, expr, p) in 
  A.SmtConstraint (c, p), Some ("_" ^ attr)

let desugar_attributes ctx ast = 
  let ast = List.map (fun element -> match element with 
  | A.ProdRule (nt, rhss, p) -> 
    let rhss = List.map (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> rhs
    | A.Rhs (ges, scs, prob, p) -> 
      let scs, new_ges = List.map (handle_sc ctx) scs |> List.split in 
      let new_ges = List.filter_map Fun.id new_ges in
      let new_ges = List.map (fun str -> A.Nonterminal (str, None, p)) new_ges in
      A.Rhs (ges @ new_ges, scs, prob, p)
    ) rhss in 
    A.ProdRule (nt, rhss, p)
  | A.TypeAnnotation _ -> element
  ) ast in 
  ast 
