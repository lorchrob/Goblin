module A = Ast
module TC = TypeChecker
module StringMap = Utils.StringMap

(* <A>.<B> + 3 < 0  
   -> 
  match <A> with 
  | <B> <C> -> <B> + 3 < 0 
  
  --------------------------
  
  <A>.<B> + <A>.<C> < 0 
   -> 
  match <A> with 
  | <B> <C> -> 
     match <A> with 
     | <B> <C> -> 
        <B> + <C> < 0
  *)

let gen_match_info ctx nt1 nt2 = 
  let rules = match StringMap.find nt1 ctx with 
  | A.ADT rules -> rules 
  | _ -> failwith "Internal error: sygus.ml (nt_to_match)" 
  in
  let rule = List.find (fun rule -> 
    List.mem nt2 rule   
  ) rules in
  let remaining_rules = List.filter (fun rule' -> rule' != rule) rules in
  let remaining_cases = List.map (fun rule -> (rule, A.BConst true)) remaining_rules in
  rule, remaining_cases

let rec nt_to_match: TC.context -> A.expr -> A.expr = 
  fun ctx expr -> 
  let r = nt_to_match ctx in 
  match expr with
  | BinOp (NTExpr (nt1 :: nt2 :: nts), op, expr2) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in
    Match (nt1, (rule, BinOp (NTExpr (nt2 :: nts), op, r expr2)) :: remaining_cases) 
  | BinOp (expr1, op, NTExpr (nt1 :: nt2 :: nts)) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in
    Match (nt1, (rule, BinOp (r expr1, op, NTExpr (nt2 :: nts))) :: remaining_cases) 
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2)
  | UnOp (op, NTExpr (nt1 :: nt2 :: nts)) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in 
    Match (nt1, (rule, UnOp (op, NTExpr (nt2 :: nts))) :: remaining_cases)
  | UnOp (op, expr) -> UnOp (op, r expr)
  | CompOp (NTExpr (nt1 :: nt2 :: nts), op, expr2) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in
    Match (nt1, (rule, CompOp (NTExpr (nt2 :: nts), op, r expr2)) :: remaining_cases) 
  | CompOp (expr1, op, NTExpr (nt1 :: nt2 :: nts)) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in
    Match (nt1, (rule, CompOp (r expr1, op, NTExpr (nt2 :: nts))) :: remaining_cases) 
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2) 
  | Length (NTExpr (nt1 :: nt2 :: nts)) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in 
    Match (nt1, (rule, Length (NTExpr (nt2 :: nts))) :: remaining_cases)
  | Length expr -> Length (r expr) 
  | Match (nt_expr, cases) -> 
    let cases = List.map (fun (nts, expr) -> 
      nts, r expr
    ) cases in
    Match (nt_expr, cases) 
  | BVCast (len, NTExpr (nt1 :: nt2 :: nts)) -> 
    let rule, remaining_cases = gen_match_info ctx nt1 nt2 in 
    Match (nt1, (rule, BVCast (len, NTExpr (nt2 :: nts))) :: remaining_cases)
  | BVCast (len, expr) -> BVCast (len, r expr)
  | NTExpr _ -> failwith "internal error: ntExprToMatch (nt_to_match)"
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | StrConst _ -> expr

let process_sc: TC.context -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx sc -> match sc with 
  | A.Dependency (nt, expr) -> Dependency (nt, nt_to_match ctx expr)
  | SyGuSExpr expr -> SyGuSExpr (nt_to_match ctx expr)

let convert_nt_exprs_to_matches: TC.context -> A.ast -> A.ast = 
  fun ctx ast -> List.map (fun element -> match element with
    | A.ProdRule (nt, rhss) -> 
      let rhss = List.map (fun rhs -> match rhs with 
      | A.Rhs (ges, scs) -> A.Rhs (ges, List.map (process_sc ctx) scs) 
      | StubbedRhs _ -> rhs 
      ) rhss in 
      A.ProdRule (nt, rhss)
    | TypeAnnotation (nt, ty, scs) -> TypeAnnotation (nt, ty, List.map (process_sc ctx) scs)
  ) ast 