module A = Ast
module TC = TypeChecker
module SM = Utils.StringMap

module SILSet = Utils.SILSet

(* <A>.<B> + 3 < 0  
   -> 
  match <A> with 
  | <B> <C> -> <B> + 3 < 0 
  
  --------------------------
  
  <A>.<B> + <A>.<C> < 0 
   -> 
  match <A> with 
  | <B> <C> -> 
    <B> + <C> < 0
  *)

  (* 
  * Could resolve ambiguities in this transformation
  * When you create a new pattern, generate fresh indices for that pattern
  * When you peel off a nonterminal from an NTExpr, rename the next nonterminal in the NTExpr based on the renamed pattern
  
  *)

let gen_match_info ast (nt1, idx1) (nt2, _idx2) nt_ctx =
  (* Collect the corresponding patterns from the AST *) 
  let rules = List.find_map (fun element -> match element with 
  | A.TypeAnnotation _ -> None 
  | A.InlinedTypeProdRule _ -> assert false
  | A.ProdRule (nt3, _, rhss, _) -> 
    if nt1 = nt3 then 
      Some (List.map (fun rhs -> match rhs with
      | A.Rhs (ges, _, _, _) -> List.map (fun ge -> match ge with 
        | A.Nonterminal (nt, idx, _, _) -> nt, idx
        | A.StubbedNonterminal (nt, _) -> nt, None
      ) ges
      | A.StubbedRhs str -> [(str, None)]
      ) rhss)
    else None
  ) ast in 
  let rules = match rules with
  | Some rules -> rules 
  | _ -> Utils.crash "sygus.ml (nt_to_match)" 
  in
  let rules' = List.filter (fun rule -> 
    List.exists (fun (nt, _) -> 
      (* TODO: Why does equality of both points of tuple cause test failure 
               (at least, at the time of writing this comment)? *)
      nt = nt2
    ) rule   
  ) rules in
  let remaining_rules = List.filter (fun rule' -> not (List.mem rule' rules')) rules in
  let rules' = List.map (List.map (fun (nt, idx) -> nt_ctx @ [nt1, idx1], (nt, idx))) rules' in
  let remaining_rules = List.map (fun nts -> 
    Ast.CaseStub (List.map (fun (nt, idx) -> nt_ctx @ [nt1, idx1], (nt, idx)) nts)
  ) remaining_rules in
  rules', remaining_rules

let rec pull_up_match_exprs: A.expr -> A.expr = 
  let r = pull_up_match_exprs in
  fun expr -> match expr with 
  | BinOp (Match (nt_ctx, nt, cases, p), op, expr2, _) -> 
    let expr2 = r expr2 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) ->
      A.Case (rule, A.BinOp (expr, op, expr2, p))  
    | A.CaseStub rule -> A.CaseStub rule 
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | BinOp (expr1, op, Match (nt_ctx, nt, cases, p), _) -> 
    let expr1 = r expr1 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.BinOp (expr1, op, expr, p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (r expr1, op, r expr2, p)
  | CompOp (Match (nt_ctx, nt, cases, p), op, expr2, _) -> 
    let expr2 = r expr2 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.CompOp (expr, op, expr2, p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | CompOp (expr1, op, Match (nt_ctx, nt, cases, p), _) -> 
    let expr1 = r expr1 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.CompOp (expr1, op, expr, p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | CompOp (expr1, op, expr2, p) -> CompOp (r expr1, op, r expr2, p)
  | UnOp (op, Match (nt_ctx, nt, cases, p), _) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.UnOp (op, expr, p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | UnOp (op, expr, p) -> UnOp (op, r expr, p)
  | BVCast (len, Match (nt_ctx, nt, cases, _), p) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.BVCast (len, expr, p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | Singleton (expr, p) -> Singleton (r expr, p)
  | BVCast (len, expr, p) -> BVCast (len, r expr, p)
  | BuiltInFunc (Length, [Match (nt_ctx, nt, cases, p)], _) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, BuiltInFunc (A.Length, [expr], p))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases, p)
  | BuiltInFunc (func, [expr], p) -> BuiltInFunc (func, [r expr], p)
  | Match (nt_ctx, nt, cases, p) -> 
    let cases = List.mapi (fun _ case -> match case with
      (* Merge redundant matching, e.g., from <A>.<B> + <A>.<C> > 0, we don't need to match on <A> twice. *)
      (* | A.Case (nts, (Match (nt_ctx2, nt2, cases2) as expr)) -> 
        if nt = nt2 then 
          match List.nth cases2 i with 
          | A.Case (_, expr) -> A.Case (nts, r expr)
          | _ -> A.Case (nts, r expr)
        else A.Case (nts, Match (nt_ctx2, nt2, cases2)) *)
      | A.Case (nts, expr) -> A.Case (nts, r expr)
      | A.CaseStub nts -> CaseStub nts
    ) cases in
    Match (nt_ctx, nt, cases, p) 
  | NTExpr _ (* -> Utils.crash "ntExprToMatch (pull_up_match_exprs)" *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ 
  | EmptySet _ -> expr
  | _ -> Utils.crash "ntExprToMatch not yet supported for some operator in your input" 

(* Recursively generate match expressions for the "top level" of each NT expr, 
   until all the NTExprs have no more dots. If we encounter the same "top level" NT 
   more than once, don't repeat the match. 
   
   Here, "top level" refers to (e.g.) <A> in <A>.<B>.<C>... 
   
   E.g., <A>.<B>.<C> > <A>.<D>.<E> 
   -> 
    match <A> with (don't match on <A> twice)
      | <A_B> <A_D> -> (we record the "match history" in the pattern names for name disambiguation)
        match <A_B> with
        | <A_B_C> -> 
          match <A_D> with 
          | <A_D_E> -> 
            <A_B_C> > <A_D_E> 
   *)
let nt_to_match: TC.context -> A.ast -> A.expr -> A.expr = 
fun ctx ast expr -> 
  let rec helper: TC.context -> A.ast -> SILSet.t -> A.expr -> SILSet.t * A.expr 
  = fun ctx ast matches_so_far expr ->
    let r = helper ctx ast in 
    match expr with
    | BinOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1, p1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2, p2), p3) -> 
      if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then 
        matches_so_far,
        BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)
      else if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && not (SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far) then
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases @ remaining_cases, p2)
      else if not (SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far) && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases, p1)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules1, remaining_cases1 = gen_match_info ast nt1 nt2 nt_ctx1 in
        let rules2, remaining_cases2 = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases1 =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules1
        in
        let cases2 =  List.fold_left (fun acc rule ->
          A.Case (rule, Match (nt_ctx1, nt1, cases1 @ remaining_cases1, p1)) :: acc
        ) [] rules2
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases2 @ remaining_cases2, p1)
      else 
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases, p1)
    | BinOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), op, expr2, p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), op, expr2, p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), op, expr2, p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1) 
    | BinOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1) 
    | BinOp (expr1, op, expr2, p1) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      BinOp (expr1, op, expr2, p1)
    | CompOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1, p1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2, p2), p3) -> 
      if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then 
        matches_so_far,
        CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)
      else if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && not (SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far) then
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases @ remaining_cases, p1)
      else if not (SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far) && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases, p1)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules1, remaining_cases1 = gen_match_info ast nt1 nt2 nt_ctx1 in
        let rules2, remaining_cases2 = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases1 =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules1
        in
        let cases2 =  List.fold_left (fun acc rule ->
          A.Case (rule, Match (nt_ctx1, nt1, cases1 @ remaining_cases1, p1)) :: acc
        ) [] rules2
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases2 @ remaining_cases2, p1)
      else 
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1, p1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2, p2), p3)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases, p1)
    | CompOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), op, expr2, p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), op, expr2, p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), op, expr2, p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1) 
    | CompOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1) 
    | CompOp (expr1, op, expr2, p1) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      CompOp (expr1, op, expr2, p1)
    | BuiltInFunc (Length, [NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1)], p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far,
        BuiltInFunc (Length, [NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1)], p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BuiltInFunc (Length, [NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1)], p2) ) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1)
    | UnOp (op, NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far,
        UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1)
    | UnOp (op, expr, p1) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      UnOp (op, expr, p1)
    | BuiltInFunc (func, [expr], p) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      BuiltInFunc (func, [expr], p) 
    | Match (nt_ctx, nt_expr, cases, p1) -> 
      let cases, matches_so_far = List.fold_left (fun (acc_cases, acc_matches) case -> match case with 
        | A.Case (nts, expr) ->
          let matches_so_far, expr = r matches_so_far expr in
          A.Case (nts, expr) :: acc_cases, SILSet.union matches_so_far acc_matches
        | A.CaseStub nts -> 
          CaseStub nts :: acc_cases, acc_matches
      ) ([], matches_so_far) cases in
      matches_so_far,
      Match (nt_ctx, nt_expr, List.rev cases, p1) 
    | BVCast (len, NTExpr (nt_ctx, nt1 :: nt2 :: nts, p1), p2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far, 
        BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts, p1), p2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases, p1)
    | Singleton (expr, p1) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far, 
      Singleton (expr, p1)
    | BVCast (len, expr, p1) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      BVCast (len, expr, p1)
    | NTExpr _ (* only reached through other rules *)
    | BVConst _ 
    | BLConst _ 
    | BConst _ 
    | IntConst _ 
    | PhConst _ 
    | StrConst _ 
    | EmptySet _ -> matches_so_far, expr
    | _ -> Utils.crash "ntExprToMatch not yet supported for some operator in your input" 
  in snd (helper ctx ast SILSet.empty expr)

(* Say a production rule has multiple options with the same nonterminal, e.g., 
  <A> ::= <B> | <B> { ... }. 
  The resolveAmbiguities step of the pipeline transformed a constraint like "<A>.<B> > 1"
  to "<A>.<B0> > 1 and <A>.<B1> > 1", because the semantics of dot notation is implicit universal 
  quantification. 

  But, the constraints over <B0> should only appear in 
  the first case of "match <A> with | <A_B0> -> ... | <A_B1> -> ...", and the constraints over 
  <B1> should only appear in the second case. So in the previous example, we should get 
  "match <A> with | <A_B0> -> <A>.<B0> > 1 | <A_B1> -> <A>.<B1> > 1"

  What if it's <A>.<B> > <A>.<B> - 1? Trivially valid, but pretend it's not. Then 
  resolveAmbiguities generates 
  <A>.<B0> > <A>.<B0> - 1 and <A>.<B0> > <A>.<B1> - 1 and <A>.<B1> > <A>.<B0> - 1 and <A>.<B1> > <A>.<B1> - 1. 
  The first case should contain <A>.<B0> > <A>.<B0> - 1, and the second should contain 
  <A>.<B1> > <A>.<B1> - 1. 
  The other two conjuncts don't fit any case, because they reference hanging identifiers.

  This function filters out the conjuncts that don't belong in each match expression case. 
  *)
let filter_out_dangling_nts expr = 
  let rec helper ctx expr = match expr with
  (* Generated conjunction. If expr1 or expr2 has dangling nts, remove it *)
  | A.BinOp (expr1, GLAnd, expr2, p) -> 
    let expr1 = helper ctx expr1 in 
    let expr2 = helper ctx expr2 in
    let b1 = A.expr_contains_dangling_nt ctx expr1 in 
    let b2 = A.expr_contains_dangling_nt ctx expr2 in
    if b1 && b2 then 
      A.BConst (true, p)
    else if b1 && not b2 then
      helper ctx expr2
    else if not b1 && b2 then
      helper ctx expr1
    else 
      A.BinOp (expr1, GLAnd, expr2, p)
  | Match (nt_ctx, nt, cases, p) -> 
    (* Extend the context on each pattern if we're matching on a valid nt *)
    if nt_ctx = [] || (SILSet.mem (nt_ctx @ [nt]) ctx) then 
      let cases = List.map (fun case -> match case with 
      | A.CaseStub _ -> case 
      | Case (nt_expr, e) -> 
        let ctx = List.fold_left (fun acc (nts, nt) ->
          SILSet.add (nts @ [nt]) acc  
        ) ctx nt_expr in 
        (* let ctx = SILSet.add (nt_ctx @ [nt]) ctx in *)
        Case (nt_expr, helper ctx e)
      ) cases in
      Match (nt_ctx, nt, cases, p) 
    (* If we match on a dangling nt, remove the match. 
       Dangling NTs in the resulting expr are handled recursively. *)
    else 
      let expr = List.find_map (fun case -> match case with 
      | A.CaseStub _ -> None 
      | Case (_, expr) -> Some expr
      ) cases |> Option.get in 
      helper ctx expr
  | A.BinOp (expr1, op, expr2, p) -> 
    A.BinOp (helper ctx expr1, op, helper ctx expr2, p) 
  | UnOp (op, expr, p) -> 
    UnOp (op, helper ctx expr, p) 
  | CompOp (expr1, op, expr2, p) -> 
    CompOp (helper ctx expr1, op, helper ctx expr2, p) 
  | BuiltInFunc (func, [expr], p) -> 
    BuiltInFunc (func, [helper ctx expr], p)
  | BVCast (len, expr, p) -> BVCast (len, helper ctx expr, p)
  | Singleton (expr, p) -> Singleton (helper ctx expr, p)
  | NTExpr _ 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ 
  | EmptySet _ -> expr
  | _ -> Utils.crash "ntExprToMatch not yet supported for some operator in your input" 
  in 
  helper SILSet.empty expr

let process_sc: TC.context -> A.ast -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx ast sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    (* let expr = 
      Utils.recurse_until_fixpoint expr (=) 
      (fun expr -> nt_to_match ctx ast expr |> pull_up_match_exprs) 
    in 
    let expr = filter_out_dangling_nts expr in *)
    (* Don't need to desugar for dependencies; they are computed outside of sygus *)
    DerivedField (nt, expr, p)
  | SmtConstraint (expr, p) -> 
    let expr = 
      Utils.recurse_until_fixpoint expr (=) 
      (fun expr -> nt_to_match ctx ast expr |> pull_up_match_exprs) 
    in 
    let expr = filter_out_dangling_nts expr in
    SmtConstraint (expr, p)
  | AttrDef _ -> assert false

let convert_nt_exprs_to_matches: TC.context -> A.ast -> A.ast = 
  fun ctx ast -> 
    List.map (fun element -> match element with
    | A.InlinedTypeProdRule _ -> assert false
    | A.ProdRule (nt, ias, rhss, p) -> 
      let rhss = List.map (fun rhs -> match rhs with 
      | A.Rhs (ges, scs, prob, p) -> A.Rhs (ges, List.map (process_sc ctx ast) scs, prob, p) 
      | A.StubbedRhs _ -> rhs 
      ) rhss in 
      A.ProdRule (nt, ias, rhss, p)
    | A.TypeAnnotation (nt, ty, scs, p) -> A.TypeAnnotation (nt, ty, List.map (process_sc ctx ast) scs, p)
  ) ast 
