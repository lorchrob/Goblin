module SA = SygusAst
module A = Ast

(* TODO:
  * Get rid of polymorphic length function stuff resulting in 
    the main function returning a list of expressions
  * Support set operations in dependency computation
*)

let eval_fail index = Utils.crash ("evaluation error #" ^ string_of_int index)

(* TODO: Replace with Utils.extract_base_name *)
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
| IntConst (i, _) -> IntLeaf i 
  | PhConst (s, _) -> VarLeaf s
  | BVConst (len, bits, _) -> BVLeaf (len, bits)
  | BLConst (bits, _) -> BLLeaf bits
  | StrConst (s, _) -> StrLeaf s
| _ -> A.pp_print_expr Format.std_formatter expr; eval_fail 1

let rec sygus_ast_to_expr: SA.sygus_ast -> A.expr list
= fun sygus_ast -> 
  match sygus_ast with 
| IntLeaf i -> [IntConst (i, Lexing.dummy_pos)]
  | BVLeaf (len, bits) -> [BVConst (len, bits, Lexing.dummy_pos)]
  | BLLeaf bits -> [BLConst (bits, Lexing.dummy_pos)]
  | VarLeaf s -> [PhConst (s, Lexing.dummy_pos)]
  | StrLeaf s -> [StrConst (s, Lexing.dummy_pos)]
  | BoolLeaf b -> [BConst (b, Lexing.dummy_pos)]
| SetLeaf _ -> Utils.crash "unsupported (sets)"
| UnitLeaf -> Utils.crash "Unexpected case"
| Node (_, sygus_asts) -> List.map sygus_ast_to_expr sygus_asts |> List.flatten

let rec compute_dep: A.semantic_constraint Utils.StringMap.t -> SA.sygus_ast -> A.ast -> A.element -> string -> SA.sygus_ast
= fun dep_map sygus_ast ast element var -> 
  match Utils.StringMap.find_opt (process_constructor_str var) dep_map with 
  | None -> 
    Utils.crash ("Hanging identifier '" ^ var ^ "' when computing dependencies")
  | Some sc -> (
    match sc with 
    | SmtConstraint _ -> Utils.crash "Encountered SmtConstraint when computing dependencies"
    | DerivedField (_, expr, _) -> 
      evaluate ~dep_map sygus_ast ast element expr |> List.hd |> expr_to_sygus_ast
  )

(* NOTE: This code assumes that the dependent term is a bitvector. If we wanted to make it general,
         we would have to have composite il_types to track the various nesting. *)
and evaluate_sygus_ast: A.semantic_constraint Utils.StringMap.t -> A.element -> A.ast -> SA.sygus_ast  -> SA.sygus_ast 
= fun dep_map element ast sygus_ast ->
  match sygus_ast with 
| IntLeaf _ | BVLeaf _ | BLLeaf _ | BoolLeaf _ | StrLeaf _ | SetLeaf _ | UnitLeaf -> sygus_ast
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
| NTExpr (_, [], _) -> Utils.crash "Unexpected case in evaluate 1"
| NTExpr (_, (id, idx0) :: rest, p) ->
  let child_index = match element with 
  | A.TypeAnnotation _ -> Some 0
  | A.ProdRule (_, rhss, _) ->
    (* Find child_index within the rule, not across all rules *) 
      List.find_map (fun rhs -> match rhs with 
      | A.StubbedRhs _ -> None 
      | Rhs (ges, _, _) ->
        Utils.find_index_opt (fun ge -> match ge with 
        | A.Nonterminal (nt, idx, _) -> 
          Utils.str_eq_ci id nt && 
          idx0 = idx 
        | StubbedNonterminal (nt, _) -> 
          Utils.str_eq_ci id nt 
        ) ges
      ) rhss
  in 
  let child_index = match child_index with 
  | Some child_index -> child_index 
  | None -> 
    let msg = Format.asprintf "Dangling identifier %s%a in semantic constraint in element %a\n"
      id 
      (fun ppf opt -> match opt with 
      | Some idx -> Format.fprintf ppf "[%d]" idx 
      | None -> Format.fprintf ppf "") idx0 
      A.pp_print_element element
    in 
    Utils.crash msg
  in
  (
  match sygus_ast with 
  | VarLeaf _ | BVLeaf _ | IntLeaf _ | BLLeaf _ | BoolLeaf _ | StrLeaf _ | SetLeaf _ | UnitLeaf -> 
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
      else child_sygus_ast |> sygus_ast_to_expr
    | Node _ when rest <> [] -> 
      (* Evaluate dot notation *)
      let element = List.find (fun element -> match element with 
      | A.ProdRule (nt, _, _) -> 
        Utils.str_eq_ci id nt
      | TypeAnnotation _ -> false
      ) ast in
      evaluate ~dep_map child_sygus_ast ast element (NTExpr ([], rest, p))
    | _ ->
      evaluate_sygus_ast dep_map element ast child_sygus_ast |> sygus_ast_to_expr
  )
| UnOp (UPlus, expr, _) -> (
  match call expr with 
  | [IntConst (i, p)] -> [IntConst (i, p)]
  | _ -> eval_fail 6
  )
| UnOp (UMinus, expr, _) -> (
  match call expr with 
  | [IntConst (i, p)] -> [IntConst (-1 * i, p)]
  | _ -> eval_fail 7
  )
| UnOp (LNot, expr, _) -> (
  match call expr with 
  | [BConst (b, p)] -> [BConst (not b, p)]
  | _ -> eval_fail 8
  )
| UnOp (BVNot, expr, _) -> (
  match call expr with 
  | [BVConst (len, b, p)] -> [BVConst (len, List.map not b, p)]
  | _ -> eval_fail 9
  )
| SeqLength (expr, _) 
| StrLength (expr, _) -> (
  match call expr with 
  | [StrConst (str, p)] -> [IntConst (String.length str, p)]
  | _ -> eval_fail 9
  )
| BinOp (expr1, BVAnd, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1, _)], [BVConst (_, bv2, _)] -> [BVConst (len, List.map2 (&&) bv1 bv2, p)] 
  | _ -> eval_fail 10
  )
| BinOp (expr1, BVOr, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1, _)], [BVConst (_, bv2, _)] -> [BVConst (len, List.map2 (||) bv1 bv2, p)]
  | _ -> eval_fail 11
  )
| BinOp (expr1, BVXor, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1, _)], [BVConst (_, bv2, _)] -> [BVConst (len, List.map2 (fun a b -> (a || b) && not (a && b)) bv1 bv2, p)] 
  | _ -> eval_fail 12
  )
| BinOp (expr1, GLAnd, expr2, p)
| BinOp (expr1, LAnd, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst (b1, _)], [BConst (b2, _)] -> [BConst (b1 && b2, p)]
  | _ -> eval_fail 13
  )
| BinOp (expr1, LOr, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst (b1, _)], [BConst (b2, _)] -> [BConst (b1 || b2, p)]
  | _ -> eval_fail 14
  )
| BinOp (expr1, LXor, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst (b1, _)], [BConst (b2, _)] -> [BConst ((b1 || b2) && not (b1 && b2), p)] 
  | _ -> eval_fail 15
  )
| BinOp (expr1, LImplies, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BConst (b1, _)], [BConst (b2, _)] -> [BConst ((not b1) || b2, p)] 
  | _ -> eval_fail 16
  )
| BinOp (expr1, Plus, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [IntConst (i1 + i2, p)] 
  | _ -> eval_fail 17
  )
| BinOp (expr1, Minus, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [IntConst (i1 - i2, p)] 
  | _ -> eval_fail 18
  )
| BinOp (expr1, Times, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [IntConst (i1 * i2, p)] 
  | _ -> eval_fail 19
  )
| BinOp (expr1, Div, expr2, p) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [IntConst (i1 / i2, p)] 
  | _ -> eval_fail 20
  )
| BinOp (expr1, StrConcat, expr2, p) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [StrConst (str1, _)], [StrConst (str2, _)] -> [StrConst (str1 ^ str2, p)] 
  | _ -> eval_fail 20
  )
| CompOp (expr1, Lt, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [BConst (i1 < i2, p)] 
  | _ -> eval_fail 21
  )
| CompOp (expr1, Lte, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [BConst (i1 <= i2, p)] 
  | _ -> eval_fail 22
  )
| CompOp (expr1, Gt, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [BConst (i1 > i2, p)] 
  | _ -> eval_fail 23
  )
| CompOp (expr1, Gte, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [BConst (i1 >= i2, p)]
  | _ -> eval_fail 24
  )
| CompOp (expr1, Eq, expr2, p) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [BConst (i1 = i2, p)]
  | [BConst (b1, _)], [BConst (b2, _)] -> [BConst (b1 = b2, p)]
  | [BVConst (_, bv1, _)], [BVConst (_, bv2, _)] -> [BConst (bv1 = bv2, p)]
  | [BLConst (bl1, _)], [BLConst (bl2, _)] -> [BConst (bl1 = bl2, p)]
  | [StrConst (str1, _)], [StrConst (str2, _)]
  | [PhConst (str1, _)], [PhConst (str2, _)] -> [BConst (String.equal str1 str2, p)]
  | _ ->
    Format.fprintf Format.std_formatter "Debug info: %a %a\n"
      (Lib.pp_print_list A.pp_print_expr "; ") expr1
      (Lib.pp_print_list A.pp_print_expr "; ") expr2;
    eval_fail 25
  )
| CompOp (expr1, BVLt, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1, _)], [BVConst (_, bv2, _)] ->
    [BConst (bvult bv1 bv2, p)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVLte, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1, _)], [BVConst (_, bv2, _)] ->
    [BConst (bvult bv1 bv2 || bv1 = bv2, p)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGt, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1, _)], [BVConst (_, bv2, _)] ->
    [BConst (bvult bv2 bv1, p)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, BVGte, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (_, bv1, _)], [BVConst (_, bv2, _)] ->
    [BConst (bvult bv2 bv1 || bv1 = bv2, p)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, StrPrefix, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [StrConst (str1, _)], [StrConst (str2, _)] ->
    let len1 = String.length str1 in
    let len2 = String.length str2 in
    [BConst (len1 <= len2 && String.sub str2 0 len1 = str1, p)] 
  | _ -> eval_fail 12
  )
| CompOp (expr1, StrContains, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [StrConst (str1, _)], [StrConst (str2, _)] ->
    (* TODO: Check this function *)
    let contains s1 s2 =
      let re = Str.regexp_string s2 in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false
    in
    [BConst (contains str1 str2, p)] 
  | _ -> eval_fail 12
  )
| Length (expr, p) -> (
  let exprs = call expr in 
  List.fold_left (fun acc expr ->
    match acc, expr with 
    | [(A.IntConst (i, _))], A.BLConst (bits, _) -> [IntConst (i + (List.length bits), p)]
    | [(A.IntConst (i, _))], A.BVConst (_, bits, _) -> [IntConst (i + (List.length bits), p)]
    | [(A.IntConst (i, _))], A.StrConst (str, _)  
    | [(A.IntConst (i, _))], A.PhConst (str, _) -> 
      if str = "<AC_TOKEN>" || str = "<SCALAR>" then [IntConst (i + 32*8, p)] 
      else if str = "<ELEMENT>" then [IntConst (i + 64*8, p)]
      else if str = "<CONFIRM_HASH>" then [IntConst (32*8, p)] 
      else if str = "<SEND_CONFIRM_COUNTER>" then [IntConst (2*8, p)] 
      else Utils.crash "Tried to compute length of unknown placeholder"
    | _ -> eval_fail 26
    ) [(A.IntConst (0, p))] exprs
  )
| BVCast (len, expr, p) -> (
  match call expr with 
  | [IntConst (i, _)] -> [A.il_int_to_bv len i p]
  | _ -> eval_fail 27
 )
| BVConst _ | BLConst _ | IntConst _ | BConst _ | PhConst _ | StrConst _ | EmptySet _ -> [expr]
| Match _ -> Utils.crash "Match not yet supported in dependency computation"
| BinOp (_, SetMembership, _, _) 
| BinOp (_, SetUnion, _, _) 
| BinOp (_, SetIntersection, _, _) 
| Singleton (_, _) ->
  Utils.crash "Set operations not yet supported in dependency computation"
| ReRange (_, _, _) | ReUnion (_, _) | ReStar (_, _) | ReConcat (_, _) | StrToRe (_, _) | StrInRe (_, _, _) -> 
  Utils.crash "Regex operations not yet supported in dependency computation"


let rec compute_deps: A.semantic_constraint Utils.StringMap.t -> A.ast -> SA.sygus_ast -> SA.sygus_ast 
= fun dep_map ast sygus_ast -> match sygus_ast with
| VarLeaf _ -> eval_fail 28
| UnitLeaf -> Utils.crash "Unexpected case"
| Node ((constructor, idx), subterms) -> 
  let subterms = 
  List.map (fun subterm -> match subterm with 
  | SygusAst.Node _ -> compute_deps dep_map ast subterm
  | VarLeaf var -> 
    let element = List.find_opt (fun element -> match element with 
    | A.TypeAnnotation (nt, _, _, _)  
    | ProdRule (nt, _, _) -> 
      Utils.str_eq_ci nt (Utils.extract_base_name constructor)
    ) ast in
    let element = match element with 
    | None -> 
      Utils.crash "compute_deps"
    | Some element -> element 
    in
    if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map
    then 
      compute_dep dep_map sygus_ast ast element var 
    else sygus_ast 
  | _ -> subterm
  ) subterms in 
  Node ((constructor, idx), subterms)
| BVLeaf _ | BLLeaf _ | IntLeaf _ | BoolLeaf _ | StrLeaf _ | SetLeaf _ -> sygus_ast
