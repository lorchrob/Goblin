module A = Ast
module TC = TypeChecker

(* To help with attributes -- 
   if no index given, still not ambiguous if every option has exactly one occurrence. 
   Then synthesize all the possible constraints. Or maybe... treat attributes specially? 

   Trying that now. *)

let came_from_attribute id = id.[0] = '%'

let rec check_nt_expr_refs ctx nts p = match nts with 
| (nt1, idx1) :: (nt2, idx2) :: tl ->
  if came_from_attribute nt2 then 
    let tl = check_nt_expr_refs ctx ((nt2, idx2) :: tl) p in
    (nt1, idx1) :: (nt2, idx2) :: List.tl tl 
  else 

  let ty = Utils.StringMap.find nt1 ctx in 
  let idx2 = (match ty with 
  | A.ADT nts -> 
    let nts = List.flatten nts in 
    let nts = List.filter (fun nt -> nt = nt2) nts in (
    match idx2 with 
    (* If no label, there should be a unique match *)
    | None -> 
      if List.length nts = 1 then 
        0 
      else 
        let msg = Format.asprintf "Nonterminal %s is ambiguous. Disambiguate with some index (e.g., <%s>[0])" nt2 nt2 in 
        Utils.error msg p
    (* If a label, the unique match should actually exist *)
    | Some idx2 -> 
      if idx2 < List.length nts then 
        idx2 
      else 
        let msg = Format.asprintf "Could not resolve nonterminal %s[%d]" nt2 idx2 in 
        Utils.error msg p
    )
  | _ -> assert false 
  ) in
  let tl = check_nt_expr_refs ctx ((nt2, Some idx2) :: tl) p in
  (nt1, idx1) :: (nt2, Some idx2) :: List.tl tl 
| nts -> nts

(* Every dot notation reference, if potentially ambiguous, should include an index (in bounds) 
   to disambiguate *)
let rec check_unambiguous_references 
= fun ctx ges expr -> 
  let call = check_unambiguous_references ctx ges in
  match expr with 
  | A.NTExpr (nt_context, (nt, idx) :: nts, p) -> 
    (* Head nt should be unambiguous and in *this* RHS *)
    let idx = if came_from_attribute nt then None else match idx with (* Set this index *)
    | Some _ -> idx
    | None -> match List.filter (function 
      | A.StubbedNonterminal _ -> false
      | A.Nonterminal (id, _, _, _) -> nt = id
      ) ges with 
      | [A.Nonterminal (_, Some idx, _, _)] -> Some idx 
      | [A.Nonterminal (_, None, _, _)] -> assert false
      | _ -> 
        let msg = Format.asprintf "Nonterminal %s is ambiguous; disambiguate with some index (e.g., <%s>[0])" nt nt in 
        Utils.error msg p
    in
    let _ = match List.find_opt (fun ge -> match ge with (* Make sure it's sane *)
    | A.StubbedNonterminal _ -> false 
    | A.Nonterminal (id, idx', _, _) -> 
      nt = id && (idx = idx' || idx = None)
    ) ges with 
    | Some _ -> () 
    | None ->
      let msg = Format.asprintf "Could not resolve nonterminal %s[%d]" nt (Option.get idx) in 
      Utils.error msg p
    in 
    (* Subsequent nt references should all work for *some* RHS *)
    let nts = check_nt_expr_refs ctx ((nt, idx) :: nts) p in 
    A.NTExpr (nt_context, nts, p)
  | A.NTExpr (_, [], _) -> assert false
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | Match _ -> assert false(* -> Match (check_nt_expr_refs prm nt_expr, cases) *)
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map call exprs, p) 
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | SynthAttr _
  | InhAttr _
  | StrConst _ -> expr

let process_sc
= fun ctx ges sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    let expr = check_unambiguous_references ctx ges expr in 
    A.DerivedField (nt, expr, p)
  | SmtConstraint (expr, p) ->
    let expr = check_unambiguous_references ctx ges expr in 
    A.SmtConstraint (expr, p)
  | AttrDef _ -> assert false

let detect_ambiguities: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, ias, rhss, p) ->
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs, prob, _) -> 
    let scs = List.map (process_sc ctx ges) scs in
    A.Rhs (ges, scs, prob, p) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, ias, rhss, p)
| TypeAnnotation (nt, ty, scs, p) -> 
  let scs = List.map (process_sc ctx [A.Nonterminal (nt, Some 0, [], p)]) scs in
  TypeAnnotation (nt, ty, scs, p)
) ast 
