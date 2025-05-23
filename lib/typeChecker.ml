open Ast

type context = il_type Utils.StringMap.t

type mode = 
| SyGuS 
| Dep


let type_checker_error mode error_msg = match mode with 
| SyGuS -> Utils.error error_msg 
| Dep -> 
  let msg = Format.asprintf "<WARNING> %s\n " error_msg in
  Utils.warning_print Format.pp_print_string Format.std_formatter msg

let build_context: ast -> ast * context
= fun ast -> 
  let ctx = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, rhss) -> 
    let options = List.map (fun rhs -> match rhs with
      | Rhs (ges, _) -> List.fold_left (fun acc ge -> match ge with 
        | Nonterminal nt 
        | StubbedNonterminal (nt, _) -> nt :: acc
      ) [] ges |> List.rev
      | StubbedRhs _ -> []
    ) rhss in
    Utils.StringMap.add nt (ADT options) acc 
  | TypeAnnotation (nt, ty, _) -> Utils.StringMap.add nt ty acc
  ) Utils.StringMap.empty ast in 
  ast, ctx

let last lst = lst |> List.rev |> List.hd

let rec infer_type_expr: context -> mode -> expr -> il_type option
= fun ctx mode expr -> match expr with 
| NTExpr (_, nt_expr) -> (
  match Utils.StringMap.find (fst (last nt_expr)) ctx with 
  | ADT _ -> 
    let msg = "Type checking error: Nonterminal '" ^ (fst (last nt_expr)) ^ "' has a composite type, but is used in some operation that requires a primitive type" in
    type_checker_error mode msg;
    None
  | ty -> Some ty
  )
| UnOp (UPlus, expr) 
| UnOp (UMinus, expr) -> 
  let _ = check_type_expr ctx mode Int expr in
  Some Int
| UnOp (LNot, expr) -> 
  let _ = check_type_expr ctx mode Bool expr in
  Some Bool
| UnOp (BVNot, expr) ->
  let inf_ty = infer_type_expr ctx mode expr in (
  match inf_ty with 
  | Some BitVector _ -> inf_ty 
  | Some inf_ty -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty Int in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | None -> None
  )
| StrLength (expr) -> 
  let _ = check_type_expr ctx mode String expr in 
  Some Int
| BinOp (expr1, BVAnd, expr2) 
| BinOp (expr1, BVOr, expr2) 
| BinOp (expr1, BVXor, expr2) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some BitVector len1, Some BitVector len2 -> 
    if not (len1 = len2) then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty (BitVector len1) in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty (BitVector len2) in 
      let error_message = "Type checking error: BitVector operation " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another of type " ^ inf_ty_str2 in
      Utils.error error_message
    else Some (BitVector len1)
  | Some BitVector len, Some inf_ty2 -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr2 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty2 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | Some inf_ty1, Some BitVector len ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | Some inf_ty1, _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = "BitVector" in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | _ -> None
  )
| BinOp (expr1, GLAnd, expr2) 
| BinOp (expr1, LAnd, expr2) 
| BinOp (expr1, LOr, expr2) 
| BinOp (expr1, LXor, expr2)  
| BinOp (expr1, LImplies, expr2) -> 
  let _ = check_type_expr ctx mode Bool expr1 in 
  let _ = check_type_expr ctx mode Bool expr2 in
  Some Bool
| BinOp (expr1, Plus, expr2) 
| BinOp (expr1, Minus, expr2) 
| BinOp (expr1, Times, expr2) 
| BinOp (expr1, Div, expr2) -> 
  let _ = check_type_expr ctx mode Int expr1 in 
  let _ = check_type_expr ctx mode Int expr2 in
  Some Int
| BinOp (expr1, StrConcat, expr2) -> 
  let _ = check_type_expr ctx mode String expr1 in 
  let _ = check_type_expr ctx mode String expr2 in
  Some String
| CompOp (expr1, Lt, expr2) 
| CompOp (expr1, Lte, expr2) 
| CompOp (expr1, Gt, expr2) 
| CompOp (expr1, Gte, expr2) ->
  let _ = check_type_expr ctx mode Int expr1 in 
  let _ = check_type_expr ctx mode Int expr2 in
  Some Bool
| CompOp (expr1, StrPrefix, expr2) ->
  let _ = check_type_expr ctx mode String expr1 in 
  let _ = check_type_expr ctx mode String expr2 in
  Some Bool
| CompOp (expr1, BVLt, expr2) 
| CompOp (expr1, BVLte, expr2) 
| CompOp (expr1, BVGt, expr2) 
| CompOp (expr1, BVGte, expr2) ->
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some BitVector len1, Some BitVector len2 -> 
    if not (len1 = len2) then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty (BitVector len1) in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty (BitVector len2) in 
      let error_message = "Type checking error: BitVector operation " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another of type " ^ inf_ty_str2 in
      Utils.error error_message
    else Some Bool
  | Some BitVector len, Some inf_ty2 -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr2 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty2 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | Some inf_ty1, Some BitVector len ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | Some inf_ty1, _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = "BitVector" in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message
  | _ -> None
  )
| CompOp (expr1, Eq, expr2) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some inf_ty1, Some inf_ty2 ->
    if not (inf_ty1 = inf_ty2) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty inf_ty1 in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty inf_ty2 in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another operand of type " ^ inf_ty_str2 in
      Utils.error error_message
    else Some Bool
  | _ -> None
  )
| Match _ -> assert false
  (* let inf_ty = Utils.StringMap.find (last nt_expr) ctx in  
  if not (inf_ty = Bool) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty inf_ty in
      let exp_ty_str = Utils.capture_output Ast.pp_print_ty Bool in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another operand of type " ^ exp_ty_str in
      Utils.error error_message
  else 
    let inf_tys = List.map (fun (_, expr) -> infer_type_expr ctx mode expr) cases in 
    if not (Lib.all_equal inf_tys) 
    then 
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let error_message = "Type checking error: case expression " ^ expr_str ^ " has cases of differing types" in
      Utils.error error_message
    else List.hd inf_tys *)
| Length expr -> (
  let inf_ty = infer_type_expr ctx mode expr in
  match inf_ty with 
  | Some BitList 
  | Some BitVector _ -> Some Int
  | Some (ADT _ as inf_ty) -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to length function " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type Int or BitVector" in 
    type_checker_error mode error_message;
    Some Int
  | Some inf_ty -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to length function " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type Int or BitVector" in 
    Utils.error error_message
  | None -> None
  )
| BVCast (len, expr)  -> 
  let _ = check_type_expr ctx mode Int expr in 
  Some (BitVector len)
| BVConst (len1, bits) -> 
  let len2 = List.length bits in
  if len1 != len2 then 
    let error_message = "Type checking error: BitVector constant with expected length " ^ string_of_int len1 ^ " has actual length " ^ string_of_int len2 in 
    Utils.error error_message
  else
    Some (BitVector len1) 
| BLConst _ -> Some BitList 
| BConst _ -> Some Bool
| IntConst _ -> Some Int
| StrConst _ -> Some String
| PhConst _ -> 
  if mode = Dep then Some Placeholder
  else 
    let error_message = "String constants can only be in dependencies (of the form 'nonterminal <- string_literal')" in 
    Utils.error error_message

and check_type_expr: context -> mode -> il_type -> expr -> expr 
= fun ctx mode exp_ty expr -> 
  match infer_type_expr ctx mode expr with 
  | None -> (* should have produced a warning *) expr
  | Some inf_ty -> 
    if not (inf_ty = exp_ty) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
      let exp_ty_str = Utils.capture_output Ast.pp_print_ty exp_ty in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
      Utils.error error_message
    else expr
  
let check_prod_rhs ctx rhss = match rhss with 
| Rhs (ges, scs) -> 
  let scs = List.map (fun sc -> match sc with 
  | Dependency (nt2, expr) -> 
    let exp_ty = 
      match Utils.StringMap.find_opt nt2 ctx with 
      | None -> 
        Utils.error "Dependency LHS must be a nonterminal with a primitive (non-inductive) type"
      | Some exp_ty -> exp_ty 
    in
    let expr = check_type_expr ctx Dep exp_ty expr in 
    Dependency (nt2, expr)
  | SyGuSExpr expr -> 
    let exp_ty = Bool in
    let expr = check_type_expr ctx SyGuS exp_ty expr in 
    SyGuSExpr expr
  ) scs in 
  Rhs (ges, scs)
| StubbedRhs _ -> assert false

let check_types: context -> ast -> ast 
= fun ctx ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, rhss) -> 
    let rhss = List.map (check_prod_rhs ctx) rhss in
    ProdRule (nt, rhss)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) ->
      let exp_ty = 
        match Utils.StringMap.find_opt nt2 ctx with 
        | None -> 
          Utils.error "Dependency LHS must be a nonterminal with a primitive (non-inductive) type"
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx Dep exp_ty expr in 
      Dependency (nt2, expr) 
    | SyGuSExpr expr -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx SyGuS exp_ty expr in 
      SyGuSExpr expr
    ) scs in 
    TypeAnnotation (nt, ty, scs)
  ) ast in 
  ast