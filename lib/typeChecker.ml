open Ast

module StringMap = Map.Make(String)
type context = il_type StringMap.t

let build_context: ast -> ast * context
= fun ast -> 
  let ctx = List.fold_left (fun acc element -> match element with 
  | ProdRule _ -> acc 
  | TypeAnnotation (nt, ty, _) -> StringMap.add nt ty acc
  ) StringMap.empty ast in 
  ast, ctx

let last lst = lst |> List.rev |> List.hd

let rec infer_type_expr: context -> expr -> il_type
= fun ctx expr -> match expr with 
| NTExpr (nt_expr, _) -> 
  StringMap.find (last nt_expr) ctx 
| UnOp (UPlus, expr) 
| UnOp (UMinus, expr) -> 
  let _ = check_type_expr ctx Int expr in
  Int
| UnOp (LNot, expr) -> 
  let _ = check_type_expr ctx Bool expr in
  Bool
| UnOp (BVNot, expr) ->
  let inf_ty = infer_type_expr ctx expr in (
  match inf_ty with 
  | BitVector _ -> inf_ty 
  | _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty Int in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    failwith error_message
  )
| BinOp (expr1, BVAnd, expr2) 
| BinOp (expr1, BVOr, expr2) 
| BinOp (expr1, BVXor, expr2) -> 
  let inf_ty1 = infer_type_expr ctx expr1 in 
  let inf_ty2 = infer_type_expr ctx expr2 in (
  match inf_ty1, inf_ty2 with 
  | BitVector len1, BitVector len2 -> 
    if not (len1 = len2) then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty (BitVector len1) in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty (BitVector len2) in 
      let error_message = "Type checking error: BitVector operation " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another of type " ^ inf_ty_str2 in
      failwith error_message
    else BitVector len1
  | BitVector len, _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr2 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty2 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    failwith error_message
  | _, BitVector len ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    failwith error_message
  | _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = "BitVector" in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    failwith error_message
  )
| BinOp (expr1, LAnd, expr2) 
| BinOp (expr1, LOr, expr2) 
| BinOp (expr1, LXor, expr2)  
| BinOp (expr1, LImplies, expr2) -> 
  let _ = check_type_expr ctx Bool expr1 in 
  let _ = check_type_expr ctx Bool expr2 in
  Bool
| BinOp (expr1, Plus, expr2) 
| BinOp (expr1, Minus, expr2) 
| BinOp (expr1, Times, expr2) 
| BinOp (expr1, Div, expr2) -> 
  let _ = check_type_expr ctx Int expr1 in 
  let _ = check_type_expr ctx Int expr2 in
  Int
| CompOp (expr1, Lt, expr2) 
| CompOp (expr1, Lte, expr2) 
| CompOp (expr1, Gt, expr2) 
| CompOp (expr1, Gte, expr2) ->
  let _ = check_type_expr ctx Int expr1 in 
  let _ = check_type_expr ctx Int expr2 in
  Bool
| CompOp (expr1, Eq, expr2) -> 
  let inf_ty1 = infer_type_expr ctx expr1 in 
  let inf_ty2 = infer_type_expr ctx expr2 in 
  if not (inf_ty1 = inf_ty2) 
  then
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in
    let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty inf_ty2 in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another operand of type " ^ inf_ty_str2 in
    failwith error_message
  else Bool
| CaseExpr (nt_expr, cases) -> 
  let inf_ty = StringMap.find (last nt_expr) ctx in  
  if not (inf_ty = Bool) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty inf_ty in
      let exp_ty_str = Utils.capture_output Ast.pp_print_ty Bool in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another operand of type " ^ exp_ty_str in
      failwith error_message
  else 
    let inf_tys = List.map (fun (_, expr) -> infer_type_expr ctx expr) cases in 
    if not (Lib.all_equal inf_tys) 
    then 
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let error_message = "Type checking error: case expression " ^ expr_str ^ " has cases of differing types" in
      failwith error_message
    else List.hd inf_tys
| Length _ -> Int
| BVCast (len, _)  -> BitVector len
| BVConst (len, _) -> BitVector len 
| BLConst _ -> BitList 
| BConst _ -> Bool
| IntConst _ -> Int

and check_type_expr: context -> il_type -> expr -> expr 
= fun ctx exp_ty expr -> 
  let inf_ty = infer_type_expr ctx expr in 
  if not (inf_ty = exp_ty) 
  then
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty exp_ty in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    failwith error_message
  else expr
  

let check_types: context -> ast -> ast 
= fun ctx ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, ges, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) -> 
      let exp_ty = 
        match StringMap.find_opt nt2 ctx with 
        | None -> 
          failwith "Dependency LHS must be a nonterminal with a primitive (non-inductive) type"
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx exp_ty expr in 
      Dependency (nt2, expr)
    | SyGuSExpr expr -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx exp_ty expr in 
      SyGuSExpr expr
    ) scs in 
    ProdRule (nt, ges, scs)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) ->
      let exp_ty = 
        match StringMap.find_opt nt2 ctx with 
        | None -> 
          failwith "Dependency LHS must be a nonterminal with a primitive (non-inductive) type"
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx exp_ty expr in 
      Dependency (nt2, expr) 
    | SyGuSExpr expr -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx exp_ty expr in 
      SyGuSExpr expr
    ) scs in 
    TypeAnnotation (nt, ty, scs)
  ) ast in 
  ast