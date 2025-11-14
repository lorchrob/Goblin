(* 
   * Attribute types are NOT inferred from RHS
     * This becomes a problem if we want to allow circular attribute definitions, 
       type inference becomes nontrivial
   * Check for dangling attribute references 
   * Mandate that if there is an attribute definition of one RHS, 
     there must be a corresponding def in all RHSs
   * Only allow attribute definition in prod rules, not type annotations
     * Supporting on type annotations would require extra technical boring work
     * They would not be inherently interesting, because they could only be some expression of the single NT
       that could be inlined at the "call site"
   * Disallow duplicate attribute definitions in a single RHS

   * Desugar SynthAttr to NTExpr
   * Desugar AttrDef to SmtConstraint, 
     add nonterminal for the attribute to the prod rule, 
     and add corresponding type annotation if one doesn't already exist

   Q: Allow circular attribute definitions?
   Q: Allow "non-derived" attribute "definitions"? 
   Q: Allow unconstrained attributes in some cases (explicitly annotated)?
*)

module A = Ast

let rec synth_attr_to_nt_expr 
= fun expr -> 
  let r = synth_attr_to_nt_expr in
  match expr with
  | A.SynthAttr (nt, attr, p) ->
    let nt1 = nt, None in 
    let nt2 = "%_" ^ attr, None in 
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
  | InhAttr _
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
  let c = A.CompOp (NTExpr ([], ["%_" ^ attr, None], p), A.Eq, expr, p) in 
  A.SmtConstraint (c, p), Some ("%_" ^ attr)

let desugar_attributes ctx ast = 
  let ast = List.map (fun element -> match element with 
  | A.ProdRule (nt, rhss, p) -> 
    let rhss = List.map (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> rhs
    | A.Rhs (ges, scs, prob, p) -> 
      (* Synthesized attributes *)
      let scs, new_ges = List.map (handle_sc ctx) scs |> List.split in 
      let new_ges = List.filter_map Fun.id new_ges in
      let new_ges = List.map (fun str -> A.Nonterminal (str, None, [], p)) new_ges in
      (* Inherited attributes *)
      let new_ges2 = _ in
      A.Rhs (ges @ new_ges @ new_ges2, scs, prob, p)
    ) rhss in 
    A.ProdRule (nt, rhss, p)
  | A.TypeAnnotation _ -> element
  ) ast in 
  ast 
