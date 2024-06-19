open Ast

let il_int_to_bitvector: int -> int -> expr 
= fun length n ->
  if n >= (1 lsl length) then
    failwith ("Tried to cast integer " ^ string_of_int n ^ " to BitVector of inadequate width " ^ string_of_int length)
  else
    let rec to_bits acc len n =
      if len = 0 then acc
      else
        let bit = (n land 1) = 1 in
        to_bits (bit :: acc) (len - 1) (n lsr 1)
    in
    let bits = to_bits [] length n in 
    BVConst (length, bits)

let rec calculate_casts: expr -> expr 
= fun expr -> match expr with 
| BVCast (len, int) ->
  il_int_to_bitvector len int
| BinOp (expr1, op, expr2) -> BinOp (calculate_casts expr1, op, calculate_casts expr2) 
| UnOp (op, expr) -> UnOp (op, calculate_casts expr) 
| CompOp (expr1, op, expr2) -> CompOp (calculate_casts expr1, op, calculate_casts expr2) 
| Length expr -> Length (calculate_casts expr) 
| CaseExpr (nt_expr, cases) -> CaseExpr (nt_expr, cases) 
| NTExpr _ 
| BVConst _ 
| BLConst _ 
| BConst _ 
| IntConst _ -> expr

let simp_ast: ast -> ast 
= fun ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, ges, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt, expr) -> Dependency (nt, calculate_casts expr)
    | SyGuSExpr expr -> SyGuSExpr (calculate_casts expr)
    ) scs in 
    ProdRule (nt, ges, scs)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt, expr) -> Dependency (nt, calculate_casts expr)
    | SyGuSExpr expr -> SyGuSExpr (calculate_casts expr)
    ) scs in 
    TypeAnnotation (nt, ty, scs)
  ) ast in 
  ast

let abstract_dependencies: ast -> ast 
= fun ast -> simp_ast ast