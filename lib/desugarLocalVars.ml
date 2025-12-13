(* Desugar local variables by generating new nonterminals for inherited
attributes *)

module A = Ast

(* For production rules with inlined type annotations for its inherited
attributes, create a new type annotation for each inherited attributes,
and desugar by adding the non-terminal as part of the name, then
converting the InlinedTypeProdRule to a regular ProdRule. *)
let desugar_local_vars ast =
  List.concat_map (fun element -> match element with
  | A.InlinedTypeProdRule (nt, typed_params, rhss, p) ->
    let new_type_annotations = List.map (fun (ia, ty) ->
      A.TypeAnnotation ("%_" ^ nt ^ "_" ^ ia, ty, [], p)
    ) typed_params in
    let rec desugar_expr expr = match expr with
      | A.InhAttr (attr, p) -> (match List.find_opt (fun (ia, _) -> ia = attr) typed_params with
        | Some _ -> A.InhAttr (nt ^ "_" ^ attr, p)
        | None -> expr
        )
      | A.BinOp (e1, op, e2, p) ->
        let e1 = desugar_expr e1 in
        let e2 = desugar_expr e2 in
        A.BinOp (e1, op, e2, p)
      | A.CompOp (e1, op, e2, p) ->
        let e1 = desugar_expr e1 in
        let e2 = desugar_expr e2 in
        A.CompOp (e1, op, e2, p)
      | A.UnOp (op, e, p) ->
        let e = desugar_expr e in
        A.UnOp (op, e, p)
      | A.BuiltInFunc (func, exprs, p) ->
        let exprs = List.map desugar_expr exprs in
        A.BuiltInFunc (func, exprs, p)
      | A.Singleton (e, p) ->
        let e = desugar_expr e in
        A.Singleton (e, p)
      | A.BVCast (len, e, p) ->
        let e = desugar_expr e in
        A.BVCast (len, e, p)
      | A.SynthAttr _
      | A.EmptySet _
      | A.Match _
      | A.NTExpr _
      | A.BVConst _
      | A.BLConst _
      | A.BConst _
      | A.IntConst _
      | A.PhConst _
      | A.StrConst _ -> expr in
    let new_rhss = List.map (fun rhs -> match rhs with
      | A.StubbedRhs _ -> assert false
      | A.Rhs (ges, scs, prob, p) ->
        let new_ges = List.map (fun ge -> match ge with
          | A.StubbedNonterminal _ -> ge
          | A.Nonterminal (nt1, idx, ias, p) ->
            let new_ias = List.map (fun ia -> desugar_expr ia) ias in
            A.Nonterminal (nt1, idx, new_ias, p)
        ) ges in
    let new_scs = List.map (fun sc -> match sc with
      | A.SmtConstraint (e, p) -> A.SmtConstraint ((desugar_expr e), p)
      | A.AttrDef _ -> sc
      | DerivedField _ -> sc) scs in
      A.Rhs (new_ges, new_scs, prob, p)
    ) rhss in
    let ias = List.map (fun (ia, _) -> nt ^ "_" ^ ia) typed_params in
     [A.ProdRule (nt, ias, new_rhss, p)] @ new_type_annotations
  | _ -> [element]
  ) ast