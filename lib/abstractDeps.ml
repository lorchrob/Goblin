open Ast

(* Module state for fresh identifiers *)
let k = ref 0

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

let stub_grammar_element: semantic_constraint list -> grammar_element -> semantic_constraint option * grammar_element 
= fun scs ge -> match ge with 
| StubbedNonterminal _ -> None, ge 
| Nonterminal nt -> (
  match List.find_opt (fun sc -> match sc with
  | SyGuSExpr _ -> false 
  | Dependency (nt2, _) -> nt = nt2
  ) scs with 
  | Some dep -> 
    let stub_id = "_stub" ^ (string_of_int !k) ^ "_" ^ nt in
    k := !k + 1;
    Some dep, StubbedNonterminal (nt, stub_id) 
  | None -> None, ge
  )
| NamedNonterminal _ -> failwith "Named nonterminals not yet supported"


let simp_ast: ast -> (semantic_constraint Utils.StringMap.t * ast) 
= fun ast -> 
  let dep_maps, ast = List.map (fun element -> match element with 
  | ProdRule (nt, ges, scs) -> 
    (* Abstract away dependent terms. Whenever we abstract away a term, we store 
       a mapping from the abstracted stub ID to the original dependency *)
    let dep_map, ges = List.fold_left (fun acc ge -> 
      match stub_grammar_element scs ge with 
      | Some dep, StubbedNonterminal (nt, stub_id) -> 
        Utils.StringMap.add stub_id dep (fst acc), snd acc @ [StubbedNonterminal (nt, stub_id)]
      | None, ge -> (fst acc), snd acc @ [ge] 
      | Some _, _ -> assert false 
    ) (Utils.StringMap.empty, []) ges in
    (* Compute BV -> int casts *)
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt, expr) -> Dependency (nt, calculate_casts expr)
    | SyGuSExpr expr -> SyGuSExpr (calculate_casts expr)
    ) scs in 
    dep_map, ProdRule (nt, ges, scs)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt, expr) -> Dependency (nt, calculate_casts expr)
    | SyGuSExpr expr -> SyGuSExpr (calculate_casts expr)
    ) scs in 
    Utils.StringMap.empty, TypeAnnotation (nt, ty, scs)
  ) ast |> List.split in 
  let dep_map = List.fold_left (Utils.StringMap.merge Lib.union_keys) Utils.StringMap.empty dep_maps in
  dep_map, ast

let abstract_dependencies: ast -> (semantic_constraint Utils.StringMap.t * ast)  
= fun ast -> simp_ast ast