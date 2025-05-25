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
  | A.ProdRule (nt3, rhss) -> 
    if nt1 = nt3 then 
      Some (List.map (fun rhs -> match rhs with
      | A.Rhs (ges, _) -> List.map (fun ge -> match ge with 
        | A.Nonterminal (nt, idx) -> nt, idx
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
  | BinOp (Match (nt_ctx, nt, cases), op, expr2) -> 
    let expr2 = r expr2 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) ->
      A.Case (rule, A.BinOp (expr, op, expr2))  
    | A.CaseStub rule -> A.CaseStub rule 
    ) cases in
    Match (nt_ctx, nt, cases)
  | BinOp (expr1, op, Match (nt_ctx, nt, cases)) -> 
    let expr1 = r expr1 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.BinOp (expr1, op, expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2)
  | CompOp (Match (nt_ctx, nt, cases), op, expr2) -> 
    let expr2 = r expr2 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.CompOp (expr, op, expr2))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | CompOp (expr1, op, Match (nt_ctx, nt, cases)) -> 
    let expr1 = r expr1 in
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.CompOp (expr1, op, expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2)
  | UnOp (op, Match (nt_ctx, nt, cases)) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.UnOp (op, expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | UnOp (op, expr) -> UnOp (op, r expr)
  | BVCast (len, Match (nt_ctx, nt, cases)) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.BVCast (len, expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | BVCast (len, expr) -> BVCast (len, r expr)
  | Length (Match (nt_ctx, nt, cases)) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.Length (expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | StrLength (expr) -> StrLength (r expr)
  | Length (expr) -> Length (r expr)
  | Match (nt_ctx, nt, cases) -> 
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
    Match (nt_ctx, nt, cases) 
  | NTExpr _ (* -> Utils.crash "ntExprToMatch (pull_up_match_exprs)" *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

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
    | BinOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2)) -> 
      if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then 
        matches_so_far,
        BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))
      else if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && not (SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far) then
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases @ remaining_cases)
      else if not (SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far) && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules1, remaining_cases1 = gen_match_info ast nt1 nt2 nt_ctx1 in
        let rules2, remaining_cases2 = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases1 =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules1
        in
        let cases2 =  List.fold_left (fun acc rule ->
          A.Case (rule, Match (nt_ctx1, nt1, cases1 @ remaining_cases1)) :: acc
        ) [] rules2
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases2 @ remaining_cases2)
      else 
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases)
    | BinOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts), op, expr2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases) 
    | BinOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases) 
    | BinOp (expr1, op, expr2) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      BinOp (expr1, op, expr2)
    | CompOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2)) -> 
      if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then 
        matches_so_far,
        CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))
      else if SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far && not (SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far) then
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases @ remaining_cases)
      else if not (SILSet.mem (nt_ctx1 @ [nt1]) matches_so_far) && SILSet.mem (nt_ctx2 @ [nt3]) matches_so_far then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let matches_so_far = SILSet.add (nt_ctx2 @ [nt3]) matches_so_far in
        let rules1, remaining_cases1 = gen_match_info ast nt1 nt2 nt_ctx1 in
        let rules2, remaining_cases2 = gen_match_info ast nt3 nt4 nt_ctx2 in
        let cases1 =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules1
        in
        let cases2 =  List.fold_left (fun acc rule ->
          A.Case (rule, Match (nt_ctx1, nt1, cases1 @ remaining_cases1)) :: acc
        ) [] rules2
        in
        matches_so_far,
        Match (nt_ctx2, nt3, cases2 @ remaining_cases2)
      else 
        let matches_so_far = SILSet.add (nt_ctx1 @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx1 in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx1, nt1, cases @ remaining_cases)
    | CompOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts), op, expr2) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases) 
    | CompOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases) 
    | CompOp (expr1, op, expr2) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      CompOp (expr1, op, expr2)
    | Length (NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far,
        Length (NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, Length (NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases)
    | UnOp (op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far,
        UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases)
    | UnOp (op, expr) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      UnOp (op, expr)
    | StrLength expr -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      StrLength (expr) 
    | Length expr -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      Length (expr) 
    | Match (nt_ctx, nt_expr, cases) -> 
      let cases, matches_so_far = List.fold_left (fun (acc_cases, acc_matches) case -> match case with 
        | A.Case (nts, expr) ->
          let matches_so_far, expr = r matches_so_far expr in
          A.Case (nts, expr) :: acc_cases, SILSet.union matches_so_far acc_matches
        | A.CaseStub nts -> 
          CaseStub nts :: acc_cases, acc_matches
      ) ([], matches_so_far) cases in
      matches_so_far,
      Match (nt_ctx, nt_expr, List.rev cases) 
    | BVCast (len, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SILSet.mem (nt_ctx @ [nt1]) matches_so_far then 
        matches_so_far, 
        BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SILSet.add (nt_ctx @ [nt1]) matches_so_far in
        let rules, remaining_cases = gen_match_info ast nt1 nt2 nt_ctx in 
        let cases =  List.fold_left (fun acc rule ->
          A.Case (rule, BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: acc
        ) [] rules
        in
        matches_so_far,
        Match (nt_ctx, nt1, cases @ remaining_cases)
    | BVCast (len, expr) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      BVCast (len, expr)
    | NTExpr _ (* -> Utils.crash "ntExprToMatch (nt_to_match)" *)
    | BVConst _ 
    | BLConst _ 
    | BConst _ 
    | IntConst _ 
    | PhConst _ 
    | StrConst _ -> matches_so_far, expr
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
  (*!! If there is no GLAnd, still need to check the top-level expression... See examples/test18 *)
  | A.BinOp (expr1, GLAnd, expr2) -> 
    let expr1 = helper ctx expr1 in 
    let expr2 = helper ctx expr2 in
    let b1 = A.expr_contains_dangling_nt ctx expr1 in 
    let b2 = A.expr_contains_dangling_nt ctx expr2 in
    if b1 && b2 then 
      A.BConst true
    else if b1 && not b2 then
      helper ctx expr2
    else if not b1 && b2 then
      helper ctx expr1
    else 
      A.BinOp (expr1, GLAnd, expr2)
  | Match (nt_ctx, nt, cases) -> 
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
      Match (nt_ctx, nt, cases) 
    (* If we match on a dangling nt, remove the match. 
       Dangling NTs in the resulting expr are handled recursively. *)
    else 
      let expr = List.find_map (fun case -> match case with 
      | A.CaseStub _ -> None 
      | Case (_, expr) -> Some expr
      ) cases |> Option.get in 
      helper ctx expr
  | A.BinOp (expr1, op, expr2) -> 
    A.BinOp (helper ctx expr1, op, helper ctx expr2) 
  | UnOp (op, expr) -> 
    UnOp (op, helper ctx expr) 
  | CompOp (expr1, op, expr2) -> 
    CompOp (helper ctx expr1, op, helper ctx expr2) 
  | StrLength expr -> 
    StrLength (helper ctx expr)
  | Length expr -> 
    Length (helper ctx expr) 
  | BVCast _
  | NTExpr _ 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr
  in 
  helper SILSet.empty expr

let process_sc: TC.context -> A.ast -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx ast sc -> match sc with 
  | A.Dependency (nt, expr) -> 
    let expr = 
      Utils.recurse_until_fixpoint expr (=) 
      (fun expr -> nt_to_match ctx ast expr |> pull_up_match_exprs) 
    in 
    let expr = filter_out_dangling_nts expr in
    Dependency (nt, expr)
  | SyGuSExpr expr -> 
    let expr = 
      Utils.recurse_until_fixpoint expr (=) 
      (fun expr -> nt_to_match ctx ast expr |> pull_up_match_exprs) 
    in 
    let expr = filter_out_dangling_nts expr in
    SyGuSExpr (expr)

let convert_nt_exprs_to_matches: TC.context -> A.ast -> A.ast = 
  fun ctx ast -> 
    List.map (fun element -> match element with
    | A.ProdRule (nt, rhss) -> 
      let rhss = List.map (fun rhs -> match rhs with 
      | A.Rhs (ges, scs) -> A.Rhs (ges, List.map (process_sc ctx ast) scs) 
      | StubbedRhs _ -> rhs 
      ) rhss in 
      A.ProdRule (nt, rhss)
    | TypeAnnotation (nt, ty, scs) -> TypeAnnotation (nt, ty, List.map (process_sc ctx ast) scs)
  ) ast 