module SA = SygusAst
module A = Ast

let eval_fail index = Utils.crash ("evaluation error #" ^ string_of_int index)

let remove_stub input = 
  let open Str in
  let re = regexp "^_stub[0-9]+_\\(.*\\)$" in
  if string_match re input 0 then
    matched_group 1 input
  else
    input

let remove_suffix input = 
  let open Str in
  let re = regexp "^\\(.*\\)_con[0-9]*$" in
  if string_match re input 0 then
    String.uppercase_ascii (matched_group 1 input)
  else
    String.uppercase_ascii input

let bvult bv1 bv2 =
  let rec compare_bits bv1 bv2 =
    match bv1, bv2 with
    | [], [] -> false
    | b1 :: t1, b2 :: t2 ->
      if b1 = b2 then compare_bits t1 t2
      else b1 < b2
    | _ -> Utils.crash "Evaluator error: Bit vector operands must be of equal length"
  in
  if List.length bv1 <> List.length bv2 then
    Utils.crash "Evaluator error: Bit vector operands must be of equal length"
  else
    compare_bits bv1 bv2

(* Constructor string created with: "_stub" ^ (string_of_int !k) ^ "_" ^ nt *)
let process_constructor_str: string -> string 
= fun input -> 
  let re = Str.regexp "\\(.*\\)_con" in
  if Str.string_match re input 0 then Str.matched_group 1 input |> String.uppercase_ascii
  else Utils.crash "Interal error (process_constructor_str): Input string does not match the required format" 

let expr_to_sygus_ast: A.expr -> SA.sygus_ast 
= fun expr -> match expr with 
| IntConst i -> IntLeaf i 
| PhConst s -> VarLeaf s
| BVConst (len, bits) -> BVLeaf (len, bits)
| BLConst bits -> BLLeaf bits 
| _ -> eval_fail 1

let rec sygus_ast_to_expr: SA.sygus_ast -> A.expr list
= fun sygus_ast -> 
  match sygus_ast with 
| IntLeaf i -> [IntConst i]
| BVLeaf (len, bits) -> [BVConst (len, bits)]
| BLLeaf bits -> [BLConst bits]
| VarLeaf s -> [PhConst s]
| StrLeaf s -> [StrConst s]
| BoolLeaf b -> [BConst b]
| Node (_, sygus_asts) -> List.map sygus_ast_to_expr sygus_asts |> List.flatten

let rec compute_dep: A.semantic_constraint Utils.StringMap.t -> SA.sygus_ast -> A.ast -> A.element -> string -> SA.sygus_ast
= fun dep_map sygus_ast ast element var -> 
  match Utils.StringMap.find_opt (process_constructor_str var) dep_map with 
  | None -> 
    Utils.crash ("Hanging identifier '" ^ var ^ "' when computing dependencies")
  | Some sc -> (
    match sc with 
    | SyGuSExpr _ -> Utils.crash "Encountered SyGuSExpr when computing dependencies"
    | Dependency (_, expr) -> 
      evaluate ~dep_map sygus_ast ast element expr |> List.hd |> expr_to_sygus_ast
  )

(* NOTE: This code assumes that the dependent term is a bitvector. If we wanted to make it general,
         we would have to have composite il_types to track the various nesting. *)
and evaluate_sygus_ast: A.semantic_constraint Utils.StringMap.t -> A.element -> A.ast -> SA.sygus_ast  -> SA.sygus_ast 
= fun dep_map element ast sygus_ast ->
  match sygus_ast with 
| IntLeaf _ | BVLeaf _ | BLLeaf _ | BoolLeaf _ | StrLeaf _ -> sygus_ast
| VarLeaf var ->
  if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map 
    then
      let sygus_ast = compute_dep dep_map sygus_ast ast element var in 
      evaluate_sygus_ast dep_map element ast sygus_ast
    else sygus_ast
| Node (cons, subterms) -> 
  let subterms = List.map (evaluate_sygus_ast dep_map element ast) subterms in 
  Node (cons, subterms)


and evaluate: ?dep_map:A.semantic_constraint Utils.StringMap.t -> SA.sygus_ast -> A.ast -> A.element -> A.expr -> A.expr list
= fun ?(dep_map=Utils.StringMap.empty) sygus_ast ast element expr -> 
  let call = evaluate ~dep_map sygus_ast ast element in
  match expr with 
| NTExpr (_, []) -> Utils.crash "Unexpected case in evaluate"
| NTExpr (_, (id, _) :: rest) -> (* TODO: Consider the index *)
  let child_index = match element with 
  | A.TypeAnnotation _ -> Utils.crash "Unexpected case in evaluate" 
  | A.ProdRule (_, (Rhs (ges, _)) :: _) -> (* TODO: Should we really be ignoring the other production rule options? *)
    (try  
      Utils.find_index (fun ge -> match ge with 
      | A.Nonterminal nt -> id = nt 
      | StubbedNonterminal (nt, _) -> id = nt;
      ) ges 
    with Not_found ->
      Utils.crash ("Dangling identifier " ^ id ^ " in semantic constraint"))
  | A.ProdRule _ -> Utils.crash "Unexpected case in evaluate"
  in (
  match sygus_ast with 
  | VarLeaf _ | BVLeaf _ | IntLeaf _ | BLLeaf _ | BoolLeaf _ | StrLeaf _ -> 
    sygus_ast_to_expr sygus_ast
  | Node (_, subterms) -> 
    let child_sygus_ast = List.nth subterms child_index in 
    match child_sygus_ast with 
    | VarLeaf var -> 
      (* If we encounter a dependency, we have to compute it first.
         Could loop infinitely! Maybe use a cache to see if we've tried to compute this before? *)
      if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map 
      then 
        let sygus_ast = compute_dep dep_map sygus_ast ast element var in 
        evaluate_sygus_ast dep_map element ast sygus_ast |> sygus_ast_to_expr
      else sygus_ast |> sygus_ast_to_expr
    | Node _ when rest <> [] -> 
      (* Evaluate dot notation *)
      let element = List.find (fun element -> match element with 
      | A.ProdRule (nt, _) -> id = nt
      | TypeAnnotation _ -> false
      ) ast in
      evaluate ~dep_map child_sygus_ast ast element (NTExpr ([], rest))
    | _ ->
      evaluate_sygus_ast dep_map element ast child_sygus_ast |> sygus_ast_to_expr
  )
| UnOp (UPlus, expr) -> (
  match call expr with 
  | [IntConst i] -> [IntConst i]
  | _ -> eval_fail 6
  )
| UnOp (UMinus, expr) -> (
  match call expr with 
  | [IntConst i] -> [IntConst (-1 * i)]
  | _ -> eval_fail 7
  )
| UnOp (LNot, expr) -> (
  match call expr with 
  | [BConst b] -> [BConst (not b)]
  | _ -> eval_fail 8
  )
| UnOp (BVNot, expr) -> (
  match call expr with 
  | [BVConst (len, b)] -> [BVConst (len, List.map not b)]
  | _ -> eval_fail 9
  )
| StrLength expr -> (
  match call expr with 
  | [StrConst str ] -> [IntConst (String.length str)]
  | _ -> eval_fail 9
  )
| BinOp (expr1, BVAnd, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1)], [BVConst (_, bv2)] -> [BVConst (len, List.map2 (&&) bv1 bv2)] 
  | _ -> eval_fail 10
  )
| BinOp (expr1, BVOr, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1)], [BVConst (_, bv2)] -> [BVConst (len, List.map2 (||) bv1 bv2)]
  | _ -> eval_fail 11
  )
| BinOp (expr1, BVXor, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1)], [BVConst (_, bv2)] -> [BVConst (len, List.map2 (fun a b -> (a || b) && not (a && b)) bv1 bv2)] 
  | _ -> eval_fail 12
  )
|  BinOp (expr1, GLAnd, expr2)
| BinOp (expr1, LAnd, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst b1], [BConst b2] -> [BConst (b1 && b2)]
  | _ -> eval_fail 13
  )
| BinOp (expr1, LOr, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst b1], [BConst b2] -> [BConst (b1 || b2)]
  | _ -> eval_fail 14
  )
| BinOp (expr1, LXor, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst b1], [BConst b2] -> [BConst ((b1 || b2) && not (b1 && b2))] 
  | _ -> eval_fail 15
  )
| BinOp (expr1, LImplies, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst b1], [BConst b2] -> [BConst ((not b1) || b2)] 
  | _ -> eval_fail 16
  )
| BinOp (expr1, Plus, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [IntConst (i1 + i2)] 
  | _ -> eval_fail 17
  )
| BinOp (expr1, Minus, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [IntConst (i1 - i2)] 
  | _ -> eval_fail 18
  )
| BinOp (expr1, Times, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [IntConst (i1 * i2)] 
  | _ -> eval_fail 19
  )
| BinOp (expr1, Div, expr2) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [IntConst (i1 / i2)] 
  | _ -> eval_fail 20
  )
| BinOp (expr1, StrConcat, expr2) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [StrConst str1], [StrConst str2] -> [StrConst (str1 ^ str2)] 
  | _ -> eval_fail 20
  )
| CompOp (expr1, Lt, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [BConst (i1 < i2)] 
  | _ -> eval_fail 21
  )
| CompOp (expr1, Lte, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [BConst (i1 <= i2)] 
  | _ -> eval_fail 22
  )
| CompOp (expr1, Gt, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [BConst (i1 > i2)] 
  | _ -> eval_fail 23
  )
| CompOp (expr1, Gte, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [BConst (i1 >= i2)]
  | _ -> eval_fail 24
  )
| CompOp (expr1, Eq, expr2) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst i1], [IntConst i2] -> [BConst (i1 = i2)]
  | [BConst b1], [BConst b2] -> [BConst (b1 = b2)]
  | [BVConst (_, bv1)], [BVConst (_, bv2)] -> [BConst (bv1 = bv2)]
  | [BLConst bl1], [BLConst bl2] -> [BConst (bl1 = bl2)]
  | _ -> eval_fail 25
  )
| CompOp (expr1, BVLt, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1)], [BVConst (_, bv2)] ->
    [BConst (bvult bv1 bv2)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVLte, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1)], [BVConst (_, bv2)] ->
    [BConst (bvult bv1 bv2 || bv1 = bv2)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGt, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1)], [BVConst (_, bv2)] ->
    [BConst (bvult bv2 bv1)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGte, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1)], [BVConst (_, bv2)] ->
    [BConst (bvult bv2 bv1 || bv1 = bv2)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, StrPrefix, expr2) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [StrConst str1], [StrConst str2] ->
    let len1 = String.length str1 in
    let len2 = String.length str2 in
    [BConst (len1 <= len2 && String.sub str2 0 len1 = str1)] 
  | _ -> eval_fail 12
  )
| Length expr -> (
  let exprs = call expr in 
  List.fold_left (fun acc expr ->
    match acc, expr with 
    | [(A.IntConst i)], A.BLConst bits -> [IntConst (i + (List.length bits))]
    | [(A.IntConst i)], A.BVConst (_, bits) -> [IntConst (i + (List.length bits))]
    | [(A.IntConst i)], A.PhConst str -> 
      if str = "<AC_TOKEN>" || str = "<SCALAR>" then [IntConst (i + 32*8)] 
      else if str = "<ELEMENT>" then [IntConst (i + 64*8)]
      else Utils.crash "Tried to compute length of unknown placeholder"
    | _ -> eval_fail 26
    ) [(A.IntConst (0))] exprs
  )
| BVCast (len, expr) -> (
  match call expr with 
  | [IntConst i] -> [A.il_int_to_bitvector len i]
  | _ -> eval_fail 27
 )
| BVConst _ | BLConst _ | IntConst _ | BConst _ | PhConst _ | StrConst _ -> [expr]
| Match _ -> Utils.crash "Match not yet supported in dependency computation"


let rec compute_deps: A.semantic_constraint Utils.StringMap.t -> A.ast -> SA.sygus_ast -> SA.sygus_ast 
= fun dep_map ast sygus_ast -> match sygus_ast with
| VarLeaf _ -> eval_fail 28
| Node (constructor, subterms) -> 
  let subterms = 
  List.map (fun subterm -> match subterm with 
  | SygusAst.Node _ -> compute_deps dep_map ast subterm
  | VarLeaf var -> 
    let element = List.find_opt (fun element -> match element with 
    | A.TypeAnnotation (nt, _, _)  
    | ProdRule (nt, _) -> 
      nt = (constructor |> remove_stub |> remove_suffix)
    ) ast in
    let element = match element with 
    | None -> 
      Utils.crash "computeDeps.ml"
    | Some element -> element 
    in
    if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map
    then 
      compute_dep dep_map sygus_ast ast element var 
    else sygus_ast 
  | _ -> subterm
  ) subterms in 
  Node (constructor, subterms)
| BVLeaf _ | BLLeaf _ | IntLeaf _ | BoolLeaf _ | StrLeaf _ -> sygus_ast