module A = Ast
module TC = TypeChecker
module SM = Utils.StringMap
module SS = Utils.StringSet

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

let gen_match_info ctx (nt1, _) (nt2, _) nt_ctx = 
  let nt_ctx = List.map fst nt_ctx in
  let rules = match SM.find nt1 ctx with 
  | A.ADT rules -> rules 
  | _ -> failwith "Internal error: sygus.ml (nt_to_match)" 
  in
  let rule = List.find (fun rule -> 
    List.mem nt2 rule   
  ) rules in
  let remaining_rules = List.filter (fun rule' -> rule' != rule) rules in
  (* let remaining_cases = List.map (fun rule -> A.CaseStub rule) remaining_rules in *)
  (*!! TODO: Generalize to possibly match multiple rules *)
  let rule = List.map (fun nt -> nt_ctx @ [nt1], nt, None) rule in
  let remaining_rules = List.map (fun nts -> 
    Ast.CaseStub (List.map (fun nt -> nt_ctx @ [nt1], nt, None) nts)
  ) remaining_rules in
  rule, remaining_rules

let rec pull_up_match_exprs: A.expr -> A.expr = 
  let r = pull_up_match_exprs in
  fun expr -> match expr with 
  | BinOp (Match (nt_ctx, nt, cases), op, expr2) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) ->
      A.Case (rule, A.BinOp (expr, op, expr2))  
    | A.CaseStub rule -> A.CaseStub rule 
    ) cases in
    Match (nt_ctx, nt, cases)
  | BinOp (expr1, op, Match (nt_ctx, nt, cases)) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.BinOp (expr1, op, expr))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2)
  | CompOp (Match (nt_ctx, nt, cases), op, expr2) -> 
    let cases = List.map (fun case -> match case with 
    | A.Case (rule, expr) -> A.Case (rule, A.CompOp (expr, op, expr2))
    | A.CaseStub rule -> A.CaseStub rule
    ) cases in
    Match (nt_ctx, nt, cases)
  | CompOp (expr1, op, Match (nt_ctx, nt, cases)) -> 
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
  | Length (expr) -> Length (r expr)
  | Match (nt_ctx, nt, cases) -> 
    let cases = List.mapi (fun i case -> match case with
      (* Merge redundant matching, e.g., from <A>.<B> + <A>.<C> > 0, we don't need to match on <A> twice. *)
      | A.Case (nts, (Match (nt_ctx2, nt2, cases2) as expr)) -> 
        if nt = nt2 then 
          match List.nth cases2 i with 
          | A.Case (_, expr) -> A.Case (nts, r expr)
          | _ -> A.Case (nts, r expr)
        else A.Case (nts, Match (nt_ctx2, nt2, cases2))
      | A.Case (nts, expr) -> A.Case (nts, r expr)
      | A.CaseStub nts -> CaseStub nts
    ) cases in
    Match (nt_ctx, nt, cases) 
  | NTExpr _ (* -> failwith "internal error: ntExprToMatch (pull_up_match_exprs)" *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | StrConst _ -> expr

(* Generate match expressions for the "top level" of each NT expr. 
   We apply this function iteratively until there are no more match 
   statements to generate. If we encounter the same "top level" NT 
   more than once, don't repeat the match. 
   
   Here, "top level" refers to (e.g.) <A> in <A>.<B>.<C>... *)
let nt_to_match: TC.context -> A.expr -> A.expr = 
fun ctx expr -> 
  let rec helper: TC.context -> SS.t -> A.expr -> SS.t * A.expr 
  = fun ctx matches_so_far expr ->
    let r = helper ctx in 
    match expr with
    | BinOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2)) -> 
      if SS.mem (fst nt1) matches_so_far && SS.mem (fst nt3) matches_so_far then 
        matches_so_far,
        BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))
      else if SS.mem (fst nt1) matches_so_far && not (SS.mem (fst nt3) matches_so_far) then
        let matches_so_far = SS.add (fst nt3) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt3 nt4 nt_ctx2 in
        matches_so_far,
        (*!! TODO: Test if the choice of nt_ctx matters *)
        Match (nt_ctx2, nt3, Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
      else if not (SS.mem (fst nt1) matches_so_far) && SS.mem (fst nt3) matches_so_far then
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx1 in
        matches_so_far,
        Match (nt_ctx1, nt1, Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far = SS.add (fst nt3) matches_so_far in
        let rule1, remaining_cases1 = gen_match_info ctx nt1 nt2 nt_ctx1 in
        let rule2, remaining_cases2 = gen_match_info ctx nt3 nt4 nt_ctx2 in
        matches_so_far,
        Match (nt_ctx2, nt3, Case (rule2, Match (nt_ctx1, nt1, Case (rule1, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases1)) :: remaining_cases2)
      else 
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx1 in
        matches_so_far,
        Match (nt_ctx1, nt1, Case (rule, BinOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
    | BinOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts), op, expr2) -> 
      if SS.mem (fst nt1) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, BinOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)) :: remaining_cases) 
    | BinOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SS.mem (fst nt1) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, BinOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: remaining_cases) 
    | BinOp (expr1, op, expr2) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      BinOp (expr1, op, expr2)
    | CompOp (NTExpr (nt_ctx1, nt1 :: nt2 :: nts1), op, NTExpr (nt_ctx2, nt3 :: nt4 :: nts2)) -> 
      if SS.mem (fst nt1) matches_so_far && SS.mem (fst nt3) matches_so_far then 
        matches_so_far,
        CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))
      else if SS.mem (fst nt1) matches_so_far && not (SS.mem (fst nt3) matches_so_far) then
        let matches_so_far = SS.add (fst nt3) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt3 nt4 nt_ctx2 in
        matches_so_far,
        Match (nt_ctx2, nt3, Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
      else if not (SS.mem (fst nt1) matches_so_far) && SS.mem (fst nt3) matches_so_far then
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx1 in
        matches_so_far,
        Match (nt_ctx1, nt1, Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
      else if not (nt_ctx1 @ [nt1] = nt_ctx2 @ [nt3]) then
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far = SS.add (fst nt3) matches_so_far in
        let rule1, remaining_cases1 = gen_match_info ctx nt1 nt2 nt_ctx1 in
        let rule2, remaining_cases2 = gen_match_info ctx nt3 nt4 nt_ctx2 in
        matches_so_far,
        Match (nt_ctx2, nt3, Case (rule2, Match (nt_ctx1, nt1, Case (rule1, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases1)) :: remaining_cases2)
      else 
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx1 in
        matches_so_far,
        Match (nt_ctx1, nt1, Case (rule, CompOp (NTExpr (nt_ctx1 @ [nt1], nt2 :: nts1), op, NTExpr (nt_ctx2 @ [nt3], nt4 :: nts2))) :: remaining_cases)
    | CompOp (NTExpr (nt_ctx, nt1 :: nt2 :: nts), op, expr2) -> 
      if SS.mem (fst nt1) matches_so_far then 
        let matches_so_far, expr2 = r matches_so_far expr2 in 
        matches_so_far,
        CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far, expr2 = r matches_so_far expr2 in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, CompOp (NTExpr (nt_ctx @ [nt1], nt2 :: nts), op, expr2)) :: remaining_cases) 
    | CompOp (expr1, op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SS.mem (fst nt1) matches_so_far then 
        let matches_so_far, expr1 = r matches_so_far expr1 in
        matches_so_far,
        CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let matches_so_far, expr1 = r matches_so_far expr1 in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, CompOp (expr1, op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: remaining_cases) 
    | CompOp (expr1, op, expr2) -> 
      let matches_so_far, expr1 = r matches_so_far expr1 in 
      let matches_so_far, expr2 = r matches_so_far expr2 in
      matches_so_far, 
      CompOp (expr1, op, expr2)
    | Length (NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SS.mem (fst nt1) matches_so_far then 
        matches_so_far,
        Length (NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in 
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, Length (NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: remaining_cases)
    | UnOp (op, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SS.mem (fst nt1) matches_so_far then 
        matches_so_far,
        UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in 
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, UnOp (op, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: remaining_cases)
    | UnOp (op, expr) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      UnOp (op, expr)
    | Length expr -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      Length (expr) 
    | Match (nt_ctx, nt_expr, cases) -> 
      let cases, matches_so_far = List.fold_left (fun (acc_cases, acc_matches) case -> match case with 
        | A.Case (nts, expr) ->
          let matches_so_far, expr = r matches_so_far expr in
          A.Case (nts, expr) :: acc_cases, SS.union matches_so_far acc_matches
        | A.CaseStub nts -> 
          CaseStub nts :: acc_cases, acc_matches
      ) ([], matches_so_far) cases in
      matches_so_far,
      Match (nt_ctx, nt_expr, List.rev cases) 
    | BVCast (len, NTExpr (nt_ctx, nt1 :: nt2 :: nts)) -> 
      if SS.mem (fst nt1) matches_so_far then 
        matches_so_far, 
        BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts))
      else
        let matches_so_far = SS.add (fst nt1) matches_so_far in
        let rule, remaining_cases = gen_match_info ctx nt1 nt2 nt_ctx in 
        matches_so_far,
        Match (nt_ctx, nt1, Case (rule, BVCast (len, NTExpr (nt_ctx @ [nt1], nt2 :: nts))) :: remaining_cases)
    | BVCast (len, expr) -> 
      let matches_so_far, expr = r matches_so_far expr in 
      matches_so_far,
      BVCast (len, expr)
    | NTExpr _ (* -> failwith "internal error: ntExprToMatch (nt_to_match)" *)
    | BVConst _ 
    | BLConst _ 
    | BConst _ 
    | IntConst _ 
    | StrConst _ -> matches_so_far, expr
  in snd (helper ctx SS.empty expr)

let process_sc: TC.context -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx sc -> match sc with 
  | A.Dependency (nt, expr) -> Dependency (nt, nt_to_match ctx expr |> pull_up_match_exprs)
  | SyGuSExpr expr -> SyGuSExpr (nt_to_match ctx expr |> pull_up_match_exprs)

let convert_nt_exprs_to_matches: TC.context -> A.ast -> A.ast = 
  fun ctx ast -> 
    Debug.debug_print Format.pp_print_string Format.std_formatter "NTExpr to Match iteration:\n";
    Debug.debug_print A.pp_print_ast Format.std_formatter ast;
    List.map (fun element -> match element with
    | A.ProdRule (nt, rhss) -> 
      let rhss = List.map (fun rhs -> match rhs with 
      | A.Rhs (ges, scs) -> A.Rhs (ges, List.map (process_sc ctx) scs) 
      | StubbedRhs _ -> rhs 
      ) rhss in 
      A.ProdRule (nt, rhss)
    | TypeAnnotation (nt, ty, scs) -> TypeAnnotation (nt, ty, List.map (process_sc ctx) scs)
  ) ast 