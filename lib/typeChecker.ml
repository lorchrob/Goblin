open Ast

type context = il_type Utils.StringMap.t

type mode = 
| SyGuS 
| Dep


let type_checker_error mode error_msg p = match mode with 
| SyGuS -> Utils.error error_msg p
| Dep -> ()

let build_context: ast -> ast * context
= fun ast -> 
  let ctx = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, rhss, _) -> 
    let options = List.map (fun rhs -> match rhs with
      | Rhs (ges, _, _) -> List.fold_left (fun acc ge -> match ge with 
        | Nonterminal (nt, _, _) 
        | StubbedNonterminal (nt, _) -> nt :: acc
      ) [] ges |> List.rev
      | StubbedRhs _ -> []
    ) rhss in
    Utils.StringMap.add nt (ADT options) acc 
  | TypeAnnotation (nt, ty, _, _) -> Utils.StringMap.add nt ty acc
  ) Utils.StringMap.empty ast in 
  ast, ctx

let last lst = lst |> List.rev |> List.hd

let rec infer_type_expr: context -> mode -> expr -> il_type option
= fun ctx mode expr -> match expr with 
| NTExpr (_, nt_expr, p) -> (
  match Utils.StringMap.find (fst (last nt_expr)) ctx with 
  | ADT _ -> 
    let msg = "Type checking error: Nonterminal '" ^ (fst (last nt_expr)) ^ "' has a composite type, but is used in some operation that requires a primitive type" in
    type_checker_error mode msg p;
    None
  | ty -> Some ty
  )
| EmptySet (ty, _) -> Some (Set ty)
| Singleton (expr, _) -> (
  let ty = infer_type_expr ctx mode expr in  
  match ty with 
  | Some ty -> Some (Set ty)
  | None -> None 
  )
| UnOp (UPlus, expr, p) 
| UnOp (UMinus, expr, p) -> 
  let _ = check_type_expr ctx mode Int expr p in
  Some Int
| UnOp (LNot, expr, p) -> 
  let _ = check_type_expr ctx mode Bool expr p in
  Some Bool
| UnOp (BVNot, expr, p) ->
  let inf_ty = infer_type_expr ctx mode expr in (
  match inf_ty with 
  | Some BitVector _ -> inf_ty 
  | Some inf_ty -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty Int in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message p
  | None -> None
  )
| SeqLength (expr, p) -> 
  let _ = check_type_expr ctx mode BitList expr p in 
  Some Int
| StrLength (expr, p) -> 
  let _ = check_type_expr ctx mode String expr p in 
  Some Int
| BinOp (expr1, BVAnd, expr2, pos) 
| BinOp (expr1, BVOr, expr2, pos) 
| BinOp (expr1, BVXor, expr2, pos) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some BitVector len1, Some BitVector len2 -> 
    if not (len1 = len2) then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty (BitVector len1) in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty (BitVector len2) in 
      let error_message = "Type checking error: BitVector operation " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another of type " ^ inf_ty_str2 in
      Utils.error error_message pos
    else Some (BitVector len1)
  | Some BitVector len, Some inf_ty2 -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr2 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty2 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message pos
  | Some inf_ty1, Some BitVector len ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message pos
  | Some inf_ty1, _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = "BitVector" in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message pos
  | _ -> None
  )
| BinOp (expr1, GLAnd, expr2, p) 
| BinOp (expr1, LAnd, expr2, p) 
| BinOp (expr1, LOr, expr2, p) 
| BinOp (expr1, LXor, expr2, p)  
| BinOp (expr1, LImplies, expr2, p) -> 
  let _ = check_type_expr ctx mode Bool expr1 p in 
  let _ = check_type_expr ctx mode Bool expr2 p in
  Some Bool
| BinOp (expr1, SetMembership, expr2, pos) -> 
  let ty1 = infer_type_expr ctx mode expr1 in 
  let ty2 = infer_type_expr ctx mode expr2 in 
  (match ty1, ty2 with 
  | Some ty1, Some (Set ty2) -> 
    if ty1 = ty2 then Some Bool else 
    let expr1_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let expr2_str = Utils.capture_output Ast.pp_print_expr expr2 in   
    let error_message = "Type checking error: expressions " ^ expr1_str ^ " and " ^ expr2_str ^ " have mismatched types for set membership query" in 
    Utils.error error_message pos
  | Some _, Some ty ->
    let ty_str = Utils.capture_output Ast.pp_print_ty ty in 
    let error_message = "Type checking error: set membership query to operand of non-set type " ^ ty_str in 
    Utils.error error_message pos
  | _ -> None)
| BinOp (expr1, SetUnion, expr2, pos) 
| BinOp (expr1, SetIntersection, expr2, pos) -> 
  let ty1 = infer_type_expr ctx mode expr1 in 
  let ty2 = infer_type_expr ctx mode expr2 in 
  (match ty1, ty2 with 
  | Some (Ast.Set ty1), Some (Set ty2) -> 
    if ty1 = ty2 then Some (Set ty1) else 
    let expr1_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let expr2_str = Utils.capture_output Ast.pp_print_expr expr2 in   
    let error_message = "Type checking error: set expressions " ^ expr1_str ^ " and " ^ expr2_str ^ " have mismatched set types" in 
    Utils.error error_message pos
  | Some (Set _), Some ty 
  | Some ty, Some (Set _) -> 
    let ty_str = Utils.capture_output Ast.pp_print_ty ty in 
    let error_message = "Type checking error: set operation applied to operand of non-set type " ^ ty_str in 
    Utils.error error_message pos
  | _ -> None)
| BinOp (expr1, Plus, expr2, p) 
| BinOp (expr1, Minus, expr2, p) 
| BinOp (expr1, Times, expr2, p) 
| BinOp (expr1, Mod, expr2, p)
| BinOp (expr1, Div, expr2, p) -> 
  let _ = check_type_expr ctx mode Int expr1 p in 
  let _ = check_type_expr ctx mode Int expr2 p in
  Some Int
| BinOp (expr1, StrConcat, expr2, p) -> 
  let _ = check_type_expr ctx mode String expr1 p in 
  let _ = check_type_expr ctx mode String expr2 p in
  Some String
| CompOp (expr1, Lt, expr2, p) 
| CompOp (expr1, Lte, expr2, p) 
| CompOp (expr1, Gt, expr2, p) 
| CompOp (expr1, Gte, expr2, p) ->
  let _ = check_type_expr ctx mode Int expr1 p in 
  let _ = check_type_expr ctx mode Int expr2 p in
  Some Bool
| CompOp (expr1, StrContains, expr2, p) 
| CompOp (expr1, StrPrefix, expr2, p) ->
  let _ = check_type_expr ctx mode String expr1 p in 
  let _ = check_type_expr ctx mode String expr2 p in
  Some Bool
| CompOp (expr1, BVLt, expr2, p) 
| CompOp (expr1, BVLte, expr2, p) 
| CompOp (expr1, BVGt, expr2, p) 
| CompOp (expr1, BVGte, expr2, p) ->
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some BitVector len1, Some BitVector len2 -> 
    if not (len1 = len2) then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty (BitVector len1) in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty (BitVector len2) in 
      let error_message = "Type checking error: BitVector operation " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another of type " ^ inf_ty_str2 in
      Utils.error error_message p
    else Some Bool
  | Some BitVector len, Some inf_ty2 -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr2 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty2 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message p
  | Some inf_ty1, Some BitVector len ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = Utils.capture_output Ast.pp_print_ty (BitVector len) in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message p
  | Some inf_ty1, _ -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr1 in
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty1 in
    let exp_ty_str = "BitVector" in 
    let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
    Utils.error error_message p
  | _ -> None
  )
| CompOp (expr1, Eq, expr2, p) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in 
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some Placeholder, Some String 
  | Some String, Some Placeholder -> Some Bool
  | Some inf_ty1, Some inf_ty2 ->
    if not (inf_ty1 = inf_ty2) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str1 = Utils.capture_output Ast.pp_print_ty inf_ty1 in
      let inf_ty_str2 = Utils.capture_output Ast.pp_print_ty inf_ty2 in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has one operand of type " ^ inf_ty_str1 ^ " and another operand of type " ^ inf_ty_str2 in
      Utils.error error_message p
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
| Length (expr, p) -> (
  let inf_ty = infer_type_expr ctx mode expr in
  match inf_ty with 
  | Some BitList 
  | Some BitVector _ -> Some Int
  | Some (ADT _ as inf_ty) -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to length function " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type Int or BitVector" in 
    type_checker_error mode error_message p;
    Some Int
  | Some inf_ty -> 
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to length function " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type Int or BitVector" in 
    Utils.error error_message p
  | None -> None
  )
| BVCast (len, expr, p)  -> 
  let _ = check_type_expr ctx mode Int expr p in 
  Some (BitVector len)
| UbvToInt (expr, p)  -> 
  let inf_ty = infer_type_expr ctx mode expr in (
  match inf_ty with 
  | Some (BitVector _) -> Some Int
  | Some inf_ty ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to `ubv_to_int` " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type BitVector" in 
    Utils.error error_message p
  | None -> None
  )
| SbvToInt (expr, p)  -> 
  let inf_ty = infer_type_expr ctx mode expr in (
  match inf_ty with 
  | Some (BitVector _) -> Some Int
  | Some inf_ty ->
    let expr_str = Utils.capture_output Ast.pp_print_expr expr in 
    let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
    let error_message = "Type checking error: Input to `sbv_to_int` " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but must have type BitVector" in 
    Utils.error error_message p
  | None -> None
  )
| BVConst (len1, bits, p) -> 
  let len2 = List.length bits in
  if len1 != len2 then 
    let error_message = "Type checking error: BitVector constant with expected length " ^ string_of_int len1 ^ " has actual length " ^ string_of_int len2 in
    Utils.error error_message p
  else
    Some (BitVector len1) 
| BLConst _ -> Some BitList 
| BConst _ -> Some Bool
| IntConst _ -> Some Int
| StrConst _ -> Some String
| PhConst (_, p) -> 
  if mode = Dep then Some Placeholder
  else 
    let error_message = "Placeholders can only be in derived fields" in 
    Utils.error error_message p
(* TODO: Add proper RegEx type *)
| ReStar (expr, p) 
| StrToRe (expr, p) -> 
  let inf_ty = infer_type_expr ctx mode expr in (
  match inf_ty with 
  | Some String -> Some String 
  | None -> None 
  | Some ty -> 
    let ty_str = Utils.capture_output Ast.pp_print_ty ty in 
    let msg = "Type checking error: str.to_re/re.* expected type String, given type " ^ ty_str in 
    Utils.error msg p
  )
| StrInRe (expr1, expr2, p) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some String, Some String -> Some Bool 
  | _, None 
  | None, _ -> None 
  | _, Some ty ->
    let ty_str = Utils.capture_output Ast.pp_print_ty ty in 
    let msg = "Type checking error: str.in_re expected type String, given type " ^ ty_str in 
    Utils.error msg p
  )
| ReRange (expr1, expr2, p) -> 
  let inf_ty1 = infer_type_expr ctx mode expr1 in
  let inf_ty2 = infer_type_expr ctx mode expr2 in (
  match inf_ty1, inf_ty2 with 
  | Some String, Some String -> Some String
  | _, None 
  | None, _ -> None 
  | _, Some ty ->
    let ty_str = Utils.capture_output Ast.pp_print_ty ty in 
    let msg = "Type checking error: str.in_re expected type String, given type " ^ ty_str in 
    Utils.error msg p
  )
| ReConcat (exprs, p) 
| ReUnion (exprs, p) -> 
  let inf_tys = List.map (infer_type_expr ctx mode) exprs in
  if List.for_all (fun ty -> match ty with 
  | Some String -> true 
  | _ -> false
  ) inf_tys then Some String 
  else if List.exists (fun ty -> match ty with 
  | None -> true 
  | _ -> false 
  ) inf_tys then None 
  else 
    let inf_ty = List.find (fun ty -> match ty with 
    | Some ty when ty <> String -> true 
    | _ -> false 
    ) inf_tys in
    let ty_str = Utils.capture_output Ast.pp_print_ty (Option.get inf_ty) in 
    let msg = "Type checking error: re.(union | ++) expected type String, given type " ^ ty_str in 
    Utils.error msg p


and check_type_expr: context -> mode -> il_type -> expr -> Lexing.position -> expr 
= fun ctx mode exp_ty expr p -> 
  match infer_type_expr ctx mode expr with 
  | None -> (* should have produced a warning *) expr
  | Some inf_ty -> 
    if not (inf_ty = exp_ty) 
    then
      let expr_str = Utils.capture_output Ast.pp_print_expr expr in
      let inf_ty_str = Utils.capture_output Ast.pp_print_ty inf_ty in
      let exp_ty_str = Utils.capture_output Ast.pp_print_ty exp_ty in 
      let error_message = "Type checking error: expression " ^ expr_str ^ " has type " ^ inf_ty_str ^ " but has expected type " ^ exp_ty_str in
      Utils.error error_message p
    else expr
  
let check_prod_rhs ctx rhss = match rhss with 
| Rhs (ges, scs, p) -> 
  let scs = List.map (fun sc -> match sc with 
  | DerivedField (nt2, expr, p) -> 
    let exp_ty = 
      match Utils.StringMap.find_opt nt2 ctx with 
      | None -> 
        Utils.error "DerivedField LHS must be a nonterminal with a primitive (non-inductive) type" p
      | Some exp_ty -> exp_ty 
    in
    let expr = check_type_expr ctx Dep exp_ty expr p in 
    DerivedField (nt2, expr, p)
  | SmtConstraint (expr, p) -> 
    let exp_ty = Bool in
    let expr = check_type_expr ctx SyGuS exp_ty expr p in 
    SmtConstraint (expr, p)
  ) scs in 
  Rhs (ges, scs, p)
| StubbedRhs _ -> assert false

let check_types: context -> ast -> ast 
= fun ctx ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, rhss, p) -> 
    let rhss = List.map (check_prod_rhs ctx) rhss in
    ProdRule (nt, rhss, p)
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (fun sc -> match sc with 
    | DerivedField (nt2, expr, p) ->
      let exp_ty = 
        match Utils.StringMap.find_opt nt2 ctx with 
        | None -> 
          Utils.error "DerivedField LHS must be a nonterminal with a primitive (non-inductive) type" p
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx Dep exp_ty expr p in 
      DerivedField (nt2, expr, p) 
    | SmtConstraint (expr, p) -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx SyGuS exp_ty expr p in 
      SmtConstraint (expr, p)
    ) scs in 
    TypeAnnotation (nt, ty, scs, p)
  ) ast in 
  ast
