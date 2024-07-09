module SA = SygusAst
module A = Ast

let eval_fail index = failwith ("Internal error: evaluation error #" ^ string_of_int index)



let bvult bv1 bv2 =
  let rec compare_bits bv1 bv2 =
    match bv1, bv2 with
    | [], [] -> false
    | b1 :: t1, b2 :: t2 ->
      if b1 = b2 then compare_bits t1 t2
      else b1 < b2
    | _ -> failwith "Evaluator error: Bit vector operands must be of equal length"
  in
  if List.length bv1 <> List.length bv2 then
    failwith "Evaluator error: Bit vector operands must be of equal length"
  else
    compare_bits bv1 bv2

(* Constructor string created with: "_stub" ^ (string_of_int !k) ^ "_" ^ nt *)
let process_constructor_str: string -> string 
= fun input -> 
  let re = Str.regexp "\\(.*\\)_con" in
  if Str.string_match re input 0 then Str.matched_group 1 input |> String.uppercase_ascii
  else failwith "Interal error (process_constructor_str): Input string does not match the required format" 

let expr_to_sygus_ast: A.expr -> SA.sygus_ast 
= fun expr -> match expr with 
| IntConst i -> IntLeaf i 
| BVConst (len, bits) -> BVLeaf (len, bits)
| BLConst bits -> BLLeaf bits 
| _ -> eval_fail 1

let sygus_ast_to_expr: SA.sygus_ast -> A.expr 
= fun sygus_ast -> match sygus_ast with 
| IntLeaf i -> IntConst i 
| BVLeaf (len, bits) -> BVConst (len, bits)
| BLLeaf bits -> BLConst bits 
| _ -> eval_fail 2

(* NOTE: This code assumes that the dependent term is a bitvector. If we wanted to make it general,
         we would have to have composite il_types to track the various nesting. *)
let rec evaluate_sygus_ast: SA.sygus_ast -> SA.sygus_ast 
= fun sygus_ast -> match sygus_ast with 
| VarLeaf _ -> failwith "Hitting a dependency that requires computation of another dependency (I think)"
| IntLeaf _ | BVLeaf _ | BLLeaf _ -> sygus_ast
| Node (_, subterms) -> 
  let subterms = List.map evaluate_sygus_ast subterms in 
  let len, subterms = List.fold_left (fun (acc_len, acc_subterms) subterm -> match subterm with
  | SA.BVLeaf (len, subterms) -> acc_len + len, acc_subterms @ subterms
  | BLLeaf subterms -> acc_len + List.length subterms, acc_subterms @ subterms
  | _ -> eval_fail 3
  ) (0, []) subterms in 
  BVLeaf (len, subterms)


let rec evaluate: SA.sygus_ast -> A.expr -> A.expr
= fun sygus_ast expr -> match expr with 
| NTExpr ([id], None) -> (
  match sygus_ast with 
  | VarLeaf _ | BVLeaf _ | IntLeaf _ | BLLeaf _ -> eval_fail 4
  | Node (_, subterms) -> (
    let find_subterm subterm = match subterm with 
    | SA.VarLeaf _ | BVLeaf _ | IntLeaf _ | BLLeaf _ -> false 
    | Node (constructor, _) -> id = (process_constructor_str constructor)  
    in
    match List.find_opt find_subterm subterms with 
    | None -> eval_fail 5
    | Some sygus_ast -> evaluate_sygus_ast sygus_ast |> sygus_ast_to_expr
    )
  )
| UnOp (UPlus, expr) -> (
  match evaluate sygus_ast expr with 
  | IntConst i -> IntConst i
  | _ -> eval_fail 6
  )
| UnOp (UMinus, expr) -> (
  match evaluate sygus_ast expr with 
  | IntConst i -> IntConst (-1 * i)
  | _ -> eval_fail 7
  )
| UnOp (LNot, expr) -> (
  match evaluate sygus_ast expr with 
  | BConst b -> BConst (not b)
  | _ -> eval_fail 8
  )
| UnOp (BVNot, expr) -> (
  match evaluate sygus_ast expr with 
  | BVConst (len, b) -> BVConst (len, List.map not b)
  | _ -> eval_fail 9
  )
| BinOp (expr1, BVAnd, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (len, bv1), BVConst (_, bv2) -> BVConst (len, List.map2 (&&) bv1 bv2) 
  | _ -> eval_fail 10
  )
| BinOp (expr1, BVOr, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (len, bv1), BVConst (_, bv2) -> BVConst (len, List.map2 (||) bv1 bv2) 
  | _ -> eval_fail 11
  )
| BinOp (expr1, BVXor, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (len, bv1), BVConst (_, bv2) -> BVConst (len, List.map2 (fun a b -> (a || b) && not (a && b)) bv1 bv2) 
  | _ -> eval_fail 12
  )
| BinOp (expr1, LAnd, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BConst b1, BConst b2 -> BConst (b1 && b2) 
  | _ -> eval_fail 13
  )
| BinOp (expr1, LOr, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BConst b1, BConst b2 -> BConst (b1 || b2) 
  | _ -> eval_fail 14
  )
| BinOp (expr1, LXor, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BConst b1, BConst b2 -> BConst ((b1 || b2) && not (b1 && b2)) 
  | _ -> eval_fail 15
  )
| BinOp (expr1, LImplies, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BConst b1, BConst b2 -> BConst ((not b1) || b2) 
  | _ -> eval_fail 16
  )
| BinOp (expr1, Plus, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> IntConst (i1 + i2) 
  | _ -> eval_fail 17
  )
| BinOp (expr1, Minus, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> IntConst (i1 - i2) 
  | _ -> eval_fail 18
  )
| BinOp (expr1, Times, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> IntConst (i1 * i2) 
  | _ -> eval_fail 19
  )
| BinOp (expr1, Div, expr2) ->
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> IntConst (i1 / i2) 
  | _ -> eval_fail 20
  )
| CompOp (expr1, Lt, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> BConst (i1 < i2) 
  | _ -> eval_fail 21
  )
| CompOp (expr1, Lte, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> BConst (i1 <= i2) 
  | _ -> eval_fail 22
  )
| CompOp (expr1, Gt, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> BConst (i1 > i2) 
  | _ -> eval_fail 23
  )
| CompOp (expr1, Gte, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> BConst (i1 >= i2) 
  | _ -> eval_fail 24
  )
| CompOp (expr1, Eq, expr2) ->
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | IntConst i1, IntConst i2 -> BConst (i1 = i2) 
  | BConst b1, BConst b2 -> BConst (b1 = b2)
  | BVConst (_, bv1), BVConst (_, bv2) -> BConst (bv1 = bv2)
  | BLConst bl1, BLConst bl2 -> BConst (bl1 = bl2)
  | _ -> eval_fail 25
  )
| CompOp (expr1, BVLt, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (_, bv1), BVConst (_, bv2) ->
    BConst (bvult bv1 bv2) 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVLte, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (_, bv1), BVConst (_, bv2) ->
    BConst (bvult bv1 bv2 || bv1 = bv2) 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGt, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (_, bv1), BVConst (_, bv2) ->
    BConst (bvult bv2 bv1) 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGte, expr2) -> 
  let expr1 = evaluate sygus_ast expr1 in 
  let expr2 = evaluate sygus_ast expr2 in (
  match expr1, expr2 with 
  | BVConst (_, bv1), BVConst (_, bv2) ->
    BConst (bvult bv2 bv1 || bv1 = bv2) 
  | _ -> eval_fail 12
  )
| Length expr -> (
  match evaluate sygus_ast expr with 
  | BLConst bits -> IntConst (List.length bits) 
  | BVConst (_, bits) -> IntConst (List.length bits)
  | _ -> eval_fail 26
  )
| BVCast (len, expr) -> (
  match evaluate sygus_ast expr with 
  | IntConst i -> Utils.il_int_to_bitvector len i
  | _ -> eval_fail 27
 )
| BVConst _ | BLConst _ | IntConst _ | BConst _ -> expr
| NTExpr _ -> failwith "Internal error: Complicated NTExprs not yet supported"
| CaseExpr _ -> failwith "Internal error: CaseExpr not yet supported"


let compute_dep: A.semantic_constraint Utils.StringMap.t -> SA.sygus_ast -> string -> SA.sygus_ast
= fun dep_map sygus_ast var -> 
  let var = process_constructor_str var in
  match Utils.StringMap.find_opt var dep_map with 
  | None -> 
    Utils.pp_print_string_map_keys Format.std_formatter dep_map;
    failwith ("Internal error: Hanging identifier '" ^ var ^ "' when computing dependencies")
  | Some sc -> (
    match sc with 
    | SyGuSExpr _ -> failwith "Internal error: Encountered SyGuSExpr when computing dependencies"
    | Dependency (_, expr) -> evaluate sygus_ast expr |> expr_to_sygus_ast
  )

let rec compute_deps: A.semantic_constraint Utils.StringMap.t -> SA.sygus_ast -> SA.sygus_ast 
= fun dep_map sygus_ast -> match sygus_ast with
| VarLeaf _ -> eval_fail 28
| Node (constructor, subterms) -> 
  Node (constructor, List.map (fun subterm -> match subterm with 
  | SygusAst.Node _ -> compute_deps dep_map subterm
  | VarLeaf var -> compute_dep dep_map sygus_ast var
  | _ -> subterm
  ) subterms)
| BVLeaf _ | BLLeaf _ | IntLeaf _ -> sygus_ast