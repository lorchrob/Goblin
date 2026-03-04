module A = Ast
module TC = TypeChecker

let cartesian_product lst1 lst2 =
  List.map (fun x -> List.map (fun y -> (x, y)) lst2) lst1 |> List.flatten

(*let rec check_nt_expr_refs ctx nts p = match nts with 
| (nt1, idx1, idx2) :: (nt2, idx3, idx4) :: tl ->
  let ty = Utils.StringMap.find nt1 ctx in 
  let idx3, idx4 = (match ty with 
  | A.ADT nts -> 
    let nts = List.flatten nts in 
    let nts = List.filter (fun nt -> nt = nt2) nts in (
    (*!! Here idx2 should be idx3 and idx4 *)
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
  (nt1, idx1, idx2) :: (nt2, Some idx3, Some idx4) :: List.tl tl 
| nts -> nts*)

(* Every dot notation reference, if potentially ambiguous, should include an index (in bounds) 
   to disambiguate *)
let rec gen_all_exprs 
= fun ctx ges expr -> 
  let r = gen_all_exprs ctx ges in
  match expr with 
  | A.NTExpr ((nt, idx1, idx2) :: (nt2, idx3, idx4) :: nts, p) -> 
    (*!! TODO: Generate errors in bad cases. Probably need a second pass. 
               Also, we should prune generated SCs that have combos of NTExprs that are mutually 
               unreachable (essentially, they contain some pair of NTExprs that has the same prefix, 
               then same NT reference with differing RHS index *)
    (* Find all possible references for idx1 and idx2 in this RHS *)
    let idx1, idx2 = List.filter (function 
      | A.StubbedNonterminal _ -> false
      | A.Nonterminal (id, Some idx', Some idx'', _, _) -> (
        match idx1, idx2 with 
        | None, None -> id = nt 
        | Some idx1, None -> id = nt && idx1 = idx' 
        | None, Some idx2 -> id = nt && idx2 = idx''
        | Some idx1, Some idx2 -> id = nt && idx1 = idx' && idx2 = idx''
      )
      | A.Nonterminal _ -> assert false
      ) ges 
      |> 
      List.map (function 
      | A.StubbedNonterminal _ -> assert false 
      | A.Nonterminal (_, Some idx', Some idx'', _, _) -> idx', idx'' 
      | A.Nonterminal _ -> assert false) 
      |> 
      List.split
    in
    (*!! Assume we have concretized nt (the head). 
         Then the second element may have a fixed RHS, if the RHS index is present. 
         If the RHS index is not present, we consider all RHSs that contain the NT index. 
         Once we have an RHS or list of RHSs, we use that to collect the ges. *)
    (* Collect all possible tails *)
    let ntss = gen_all_exprs ctx new_ges (NTExpr (nts, p)) in 
    let ntss = List.map (function
    | A.NTExpr (nts, _) -> 
      (* We recursively generate fully disambiguated (indexed) NTExprs, so `Option.get` 
         should not fail *)
      List.map (fun (nt, idx1, idx2) -> nt, Option.get idx1, Option.get idx2) nts 
    | _ -> assert false 
    ) ntss
    in
    let head_options = List.map (fun (idx1, idx2) -> nt, idx1, idx2) (List.combine idx1 idx2) in
    let nt_exprs = cartesian_product head_options ntss in
    List.map (fun (nt_head, nt_tl) -> 
      (* Re-wrap indices with Some to satisfy OCaml's type checker *)
      A.NTExpr (List.map (fun (nt, idx1, idx2) -> nt, Some idx1, Some idx2) (nt_head :: nt_tl), p)
    ) nt_exprs 
  | A.NTExpr ((nt, idx1, idx2) :: [], p) -> stub (*!! Same handling as head for previous case *)
  | A.NTExpr _ -> assert false
  | BinOp (expr1, op, expr2, p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.BinOp (e1, op, e2, p)) pairs
  | UnOp (op, expr, p) ->
    let exprs = r expr in 
    List.map (fun e -> A.UnOp (op, e, p)) exprs
  | CompOp (expr1, op, expr2, p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.CompOp (e1, op, e2, p)) pairs 
  | BuiltInFunc (func, [expr], p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.BuiltInFunc (func, [e], p)) exprs 
  | BuiltInFunc (func, [expr1; expr2], p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.BuiltInFunc (func, [e1; e2], p)) pairs 
  | Singleton (expr, p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.Singleton (e, p)) exprs
  | BVCast (i, expr, p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.BVCast (i, e, p)) exprs 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _ 
  | EmptySet _ -> [expr]
  | BuiltInFunc _ ->
    let msg = Format.asprintf "Bad function arity in expression %a" 
      A.pp_print_expr expr in 
    Utils.error msg (A.pos_of_expr expr)
  | InhAttr _
  | SynthAttr _ -> assert false

let process_sc
= fun ctx ges sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    let exprs = gen_all_exprs ctx ges expr in (
    match exprs with 
    | [] ->
      let msg = Format.asprintf "Derived field %s contains some nonterminal reference that could not be evaluated. For example, if nonterminal <nt> has only one production rule, then `<nt>@1` cannot be evaluated (use <nt>@0 instead)." nt in 
      Utils.error msg p
    | [expr] -> [A.DerivedField (nt, expr, p)]
    | _ -> 
      let msg = Format.asprintf "Derived field %s is defined ambiguously. More concretely, the definition of %s contains some nonterminal expression <nt_1>.<nt_2>...<nt_n> where some <nt_i> has multiple occurrences in its production rule (and hence the nonterminal expression could evaluate to more than one term, depending on which occurrence you pick)." 
      nt nt in 
      Utils.error msg p
    )
  | SmtConstraint (expr, p) ->
    let exprs = gen_all_exprs ctx ges expr in 
    List.map (fun expr -> A.SmtConstraint (expr, p)) exprs
  | AttrDef _ -> assert false

let resolve_ambiguities: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, ias, rhss, p) ->
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs, prob, _) -> 
    let scs = List.map (process_sc ctx ges) scs in
    A.Rhs (ges, List.flatten scs, prob, p) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, ias, rhss, p)
| TypeAnnotation (nt, ty, scs, p) -> 
  let scs = List.map (process_sc ctx [A.Nonterminal (nt, Some 0, Some 0, [], p)]) scs in
  TypeAnnotation (nt, ty, List.flatten scs, p)
) ast 
