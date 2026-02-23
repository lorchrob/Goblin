module SA = SolverAst
module A = Ast

(* TODO:
  * Get rid of polymorphic length function stuff resulting in 
    the main function returning a list of expressions
  * Support set operations in derived fields
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

let expr_to_solver_ast: A.expr -> SA.solver_ast 
= fun expr -> match expr with 
| IntConst (i, _) -> IntLeaf i 
  | PhConst (s, _) -> VarLeaf s
  | BVConst (len, bits, _) -> BVLeaf (len, bits)
  | BLConst (bits, _) -> BLLeaf bits
  | StrConst (s, _) -> StrLeaf s
  | BConst (b, _) -> BoolLeaf b
| _ -> A.pp_print_expr Format.std_formatter expr; eval_fail 1

let rec solver_ast_to_expr: bool -> SA.solver_ast -> A.expr list
= fun computing_length solver_ast -> 
  match solver_ast with 
| IntLeaf i -> [IntConst (i, Lexing.dummy_pos)]
  | BVLeaf (len, bits) -> [BVConst (len, bits, Lexing.dummy_pos)]
  | BLLeaf bits -> [BLConst (bits, Lexing.dummy_pos)]
  | VarLeaf s -> [PhConst (s, Lexing.dummy_pos)]
  | StrLeaf s -> [StrConst (s, Lexing.dummy_pos)]
  | BoolLeaf b -> [BConst (b, Lexing.dummy_pos)]
| SetLeaf _ -> Utils.error_no_pos "Sets are not yet supported in derived fields"
| UnitLeaf -> Utils.crash "Unexpected case"
| Node ((id, _), solver_asts) -> 
  if computing_length && id.[0] = '_' then 
    []  
  else 
    (List.map (solver_ast_to_expr computing_length) solver_asts |> List.flatten)

let rec compute_dep: A.semantic_constraint Utils.StringMap.t -> SA.solver_ast -> A.ast -> A.element -> string -> SA.solver_ast
= fun dep_map solver_ast ast _element var -> 
  if !Flags.debug then 
    Format.printf "compute_dep: computing dependency for %s in solver_ast %a\n"
      var
      SA.pp_print_solver_ast solver_ast;
  match Utils.StringMap.find_opt (process_constructor_str var) dep_map with 
  | None -> 
    Utils.crash ("Hanging identifier '" ^ var ^ "' when computing dependencies")
  | Some sc -> (
    match sc with 
    | SmtConstraint _ -> Utils.crash "Encountered SmtConstraint when computing dependencies"
    | AttrDef _ -> assert false
    | DerivedField (_, expr, _) -> 
      (* Hacky workaround. Brittle. Need to refactor. Doesn't generalize. 
         The problem is that when computing this new dependency, we may have to 
         be at a different element in the input AST. Juggling the input AST and the 
         solver AST to evalute is tricky in general. 
         Furthermore, the dep_map does not have enough information -- the string key 
         could, in principle, map to different dependencies in different places in the 
         input grammar. 

         We need a refactoring that will put enough information in the solver_ast 
         to evaluate without tracking the input AST.
         *)
      let element' = List.find (fun element -> match element with 
      | A.TypeAnnotation _ -> false 
      | A.ProdRule (_, _, rhss, _) ->
        List.exists (fun rhs -> match rhs with 
        | A.StubbedRhs _ -> false 
        | A.Rhs (_, scs, _, _) -> 
          List.exists (fun sc' -> sc' = sc) scs
        ) rhss 
      ) ast in
      if !Flags.debug then 
        Format.printf "The dependency computation is under element %a\n\n" 
          A.pp_print_element element';
      evaluate ~dep_map false solver_ast ast element' expr |> List.hd |> expr_to_solver_ast
  )

and bool_list_to_il_int (signed : bool) (bits : bool list) p : A.expr =
  (* convert list of bools (MSB first) to int, treating them as 0/1 bits *)
  let rec to_uint acc = function
    | [] -> acc
    | b :: bs -> to_uint ((acc lsl 1) lor (if b then 1 else 0)) bs
  in
  let n = List.length bits in
  let unsigned_val = to_uint 0 bits in
  if signed && n > 0 && List.hd bits then
    (* negative number in two's complement *)
    A.IntConst (unsigned_val - (1 lsl n), p)
  else
    A.IntConst (unsigned_val, p)

(* Input boolean denotes whether we are in the context of computing a length(.) expression *)
and evaluate: ?dep_map:A.semantic_constraint Utils.StringMap.t -> bool -> SA.solver_ast -> A.ast -> A.element -> A.expr -> A.expr list
= fun ?(dep_map=Utils.StringMap.empty) computing_length solver_ast ast element expr -> 
  if !Flags.debug then 
    Format.printf "Evaluating expression %a under element %a and solver_ast %a\n\n"
      A.pp_print_expr expr 
      A.pp_print_element element
      SA.pp_print_solver_ast solver_ast;
  let call = evaluate ~dep_map computing_length solver_ast ast element in
  match expr with 
| NTExpr (_, [], _) -> Utils.crash "Unexpected case in evaluate 1"
| NTExpr (_, (id, idx0) :: rest, p) ->
  let child_index, child_element = match element with 
  | A.TypeAnnotation _ -> 0, element
  | A.ProdRule (_, _, rhss, _) ->
    (* Find child_index within the rule, not across all rules *) 
      let ci = List.find_map (fun rhs -> match rhs with 
      | A.StubbedRhs _ -> None 
      | Rhs (ges, _, _, _) ->
        Utils.find_index_opt (fun ge -> match ge with 
        | A.Nonterminal (nt, idx, _, _) -> 
          Utils.str_eq_ci id nt && 
          (idx0 = idx) 
        | StubbedNonterminal (nt, _) -> 
          Utils.str_eq_ci id nt 
        ) ges
      ) rhss in 
      Option.get ci, 
      List.find (fun element -> match element with 
      | A.TypeAnnotation (id2, _, _, _)
      | A.ProdRule (id2, _, _, _) -> Utils.str_eq_ci id id2
      ) ast 
  in 
  (
  match solver_ast with  
  | VarLeaf _ | BVLeaf _ | IntLeaf _ | BLLeaf _ | BoolLeaf _ | StrLeaf _ | SetLeaf _ | UnitLeaf 
  | SA.Node (_, ([BVLeaf _] | [BLLeaf _] | [IntLeaf _] | [BoolLeaf _] | [StrLeaf _] | [SetLeaf _])) ->
    solver_ast_to_expr computing_length solver_ast
  | Node ((_id, _), subterms) ->
    if !Flags.debug then 
      Format.printf "nth: Looking for child_index %d in solver_ast %a\n" 
        child_index 
        SA.pp_print_solver_ast solver_ast;
    let child_solver_ast = List.nth subterms child_index in 
    match child_solver_ast with 
    | Node (_, [VarLeaf var]) -> 
      (* If we encounter a dependency, we have to compute it first.
         Could loop infinitely! Maybe use a cache to see if we've tried to compute this before? *)
      if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map 
      then 
        let solver_ast = compute_dep dep_map solver_ast ast child_element var in 
        compute_deps dep_map ast solver_ast |> solver_ast_to_expr computing_length 
      else child_solver_ast |> solver_ast_to_expr computing_length 
    | Node _ when rest <> [] -> 
      evaluate ~dep_map computing_length child_solver_ast ast child_element (NTExpr ([], rest, p))
    | _ ->
      compute_deps dep_map ast child_solver_ast |> solver_ast_to_expr computing_length
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
| BuiltInFunc (SeqLength, [expr], _) -> (
  match call expr with 
  | [A.BLConst (bits, p)] -> [IntConst (List.length bits, p)]
  | _ -> eval_fail 29
  )
| BuiltInFunc (StrLength, [expr], _) -> (
  match call expr with 
  | [StrConst (str, p)] -> [IntConst (String.length str, p)]
  | _ -> eval_fail 10 
  )
| BinOp (expr1, BVAnd, expr2, p) -> 
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [BVConst (len, bv1, _)], [BVConst (_, bv2, _)] -> [BVConst (len, List.map2 (&&) bv1 bv2, p)] 
  | _ -> eval_fail 11
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
| BinOp (expr1, Mod, expr2, p) ->
  let expr1 = call expr1 in 
  let expr2 = call expr2 in (
  match expr1, expr2 with 
  | [IntConst (i1, _)], [IntConst (i2, _)] -> [IntConst (i1 mod i2, p)] 
  | _ -> eval_fail 20
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
| BuiltInFunc (Length, [expr], p) -> (
  let exprs = evaluate ~dep_map true solver_ast ast element expr in
  let r = 
  List.fold_left (fun acc expr ->
    match acc, expr with 
    | [(A.IntConst (i, _))], A.BLConst (bits, _) -> [IntConst (i + (List.length bits), p)]
    | [(A.IntConst (i, _))], A.BVConst (_, bits, _) -> [IntConst (i + (List.length bits), p)]
    | [(A.IntConst (i, _))], A.BConst _ -> [IntConst (i + 1, p)]
    | [(A.IntConst (i, _))], A.IntConst _ -> 
      Utils.warning "Tried to compute the length of an Int value; using zero";
      [IntConst (i, p)]
    | [(A.IntConst (i, _))], A.StrConst (str, _)  
    | [(A.IntConst (i, _))], A.PhConst (str, _) -> 
      if str = "<AC_TOKEN>" || str = "<SCALAR>" then [IntConst (i + 32*8, p)] 
      else if str = "<ELEMENT>" then [IntConst (i + 64*8, p)]
      else if str = "<CONFIRM_HASH>" then [IntConst (32*8, p)] 
      else if str = "<SEND_CONFIRM_COUNTER>" then [IntConst (2*8, p)] 
      else (
        [IntConst (i + (String.length str * 8), p)]
      )
    | _ -> Format.printf "Unexpected expr: %a\n" A.pp_print_expr expr; eval_fail 26
    ) [(A.IntConst (0, p))] exprs in 
    (*Format.printf "Computed length of %a, got sub-expressions %a, and total value of %a\n"
      A.pp_print_expr expr 
      (Lib.pp_print_list A.pp_print_expr "; ") exprs 
      (Lib.pp_print_list A.pp_print_expr "; ") r;*)
    r 
  )
| BVCast (len, expr, p) -> (
  match call expr with 
  | [IntConst (i, _)] -> [A.il_int_to_bv len i p]
  | e -> Format.printf "e: %a\n" A.pp_print_expr (List.hd (List.tl e)); eval_fail 27
 )
| BuiltInFunc (UbvToInt, [expr], p) -> (
  match call expr with 
  | [BVConst (_, i, _) ] -> [bool_list_to_il_int false i p]
  | _ -> eval_fail 28
) 
| BuiltInFunc (SbvToInt, [expr], p) -> (
  match call expr with 
  | [BVConst (_, i, _)] -> [bool_list_to_il_int true i p]
  | _ -> eval_fail 29
)
| BuiltInFunc (Repeat, [expr1; expr2], p) -> (
  match call expr1, call expr2 with 
  | [IntConst (i, _)], [BConst (b, _)] -> 
    Format.printf "Evaluated repeat(%a, %a) to %a\n" 
      A.pp_print_expr expr1 
      A.pp_print_expr expr2 
      (Lib.pp_print_list Format.pp_print_bool "; ") (Utils.replicate b i);
      [A.BLConst (Utils.replicate b i, p)]
  | _ -> eval_fail 30 
)
| BVConst _ | BLConst _ | IntConst _ | BConst _ | PhConst _ | StrConst _ | EmptySet _ -> [expr]
| Match (_, _, _, p) -> Utils.error "Match not yet supported in derived fields" p
| e -> 
  let msg = Format.asprintf "Expression %a not yet supported in the evaluator (try making this non-derived)" 
    A.pp_print_expr e in 
  Utils.error msg (A.pos_of_expr e) 


and compute_deps: A.semantic_constraint Utils.StringMap.t -> A.ast -> SA.solver_ast -> SA.solver_ast 
= fun dep_map ast solver_ast -> 
  if !Flags.debug then 
    Format.printf "compute_deps with solver_ast %a\n"
      (*A.pp_print_ast ast *)
      SA.pp_print_solver_ast solver_ast;
  match solver_ast with
| VarLeaf _ -> eval_fail 28
| UnitLeaf -> Utils.crash "Unexpected case"
| Node ((constructor, idx), subterms) -> 
  let subterms = 
  List.map (fun subterm -> match subterm with 
  | SA.Node (_hd, [VarLeaf var]) -> 
    let element = List.find_opt (fun element -> match element with 
    | A.TypeAnnotation (nt, _, _, _)  
    | ProdRule (nt, _, _, _) -> 
      Utils.str_eq_ci nt (Utils.extract_base_name constructor)
    ) ast in
    let element = match element with 
    | None -> 
      Utils.crash "compute_deps"
    | Some element -> element 
    in
    if Utils.StringMap.mem (remove_suffix var |> String.uppercase_ascii) dep_map
    then 
      SA.Node (_hd, [compute_dep dep_map solver_ast ast element var]) 
    else solver_ast 
  | SA.Node (_, ([BVLeaf _] | [BLLeaf _] | [IntLeaf _] | [BoolLeaf _] | [StrLeaf _] | [SetLeaf _])) -> subterm
  | SA.Node _ -> compute_deps dep_map ast subterm
  | _ -> subterm
  ) subterms in 
  Node ((constructor, idx), subterms)
| _ -> solver_ast
