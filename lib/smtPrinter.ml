(* 
   TODO: 
     * Fix set-logic warnings when sygus is used 
*)

module A = Ast
module TC = TypeChecker
module StringMap = Utils.StringMap

let rec pp_print_ty: Format.formatter -> A.il_type -> unit 
= fun ppf ty -> match ty with 
| Unit -> Format.fprintf ppf "Unit" 
| Int -> Format.fprintf ppf "Int"
| Bool -> Format.fprintf ppf "Bool"
| Placeholder -> Format.fprintf ppf "String"
| String -> Format.fprintf ppf "String"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| ADT _ -> 
  (*Format.printf "%a\n"
    (Lib.pp_print_list (fun _ppf ss -> Format.printf "%a" (Lib.pp_print_list Format.pp_print_string ", ") ss) "; ") sss;*)
    Utils.crash "smtPrinter.ml (pp_print_ty)"
| Set ty -> Format.fprintf ppf "(Set %a)" pp_print_ty ty

let pp_print_unop: Format.formatter -> A.unary_operator -> unit 
= fun ppf op -> match op with 
| A.UPlus -> ()
| UMinus -> Format.fprintf ppf "-"
| LNot -> Format.fprintf ppf "not"
| BVNot -> Format.fprintf ppf "bvnot"

let pp_print_binop: Format.formatter -> A.bin_operator -> unit 
= fun ppf op -> match op with 
| A.BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| GLAnd
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies ->Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "div"
| Mod -> Format.fprintf ppf "mod"
| StrConcat -> Format.fprintf ppf "str.++"
| SetMembership 
| SetUnion 
| SetIntersection
| BVXor -> Utils.crash "Should not reach this case in pp_print_binop"

let pp_print_compop: Format.formatter -> A.comp_operator -> unit 
= fun ppf op -> match op with 
| A.Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="
| _ -> assert false

let pp_print_nt_helper 
= fun ppf (str, idx1, idx2) -> 
  Format.fprintf ppf "%s%s"
    (String.lowercase_ascii str) 
    (match idx1, idx2 with 
    | None, None -> ""
    | Some i, None | None, Some i -> "!" ^ string_of_int i
    | Some i, Some j -> "!" ^ string_of_int i ^ "!" ^ string_of_int j)

let rec pp_print_expr: ?nt_prefix:string -> TC.context -> Format.formatter -> A.expr -> unit 
= fun ?(nt_prefix="") ctx ppf expr -> 
  let r = pp_print_expr ~nt_prefix ctx in
  match expr with 
  | NTExpr (nts, _) ->
    let nts = List.map (fun (str, idx1, idx2) -> String.lowercase_ascii str, idx1, idx2) nts in
    (if not (String.equal nt_prefix "") then
      Format.pp_print_string ppf (nt_prefix ^ "_"));
    Lib.pp_print_list pp_print_nt_helper "_" ppf nts
  | ActLit (nt_expr, _) ->
    Format.fprintf ppf "%a_actlit" 
      r nt_expr
  | BinOp (expr1, SetMembership, expr2, _) ->
    Format.fprintf ppf "(set.member %a %a)"
      r expr1 
      r expr2
  | BinOp (expr1, SetUnion, expr2, _) ->
    Format.fprintf ppf "(set.union %a %a)"
      r expr1 
      r expr2
  | BinOp (expr1, SetIntersection, expr2, _) ->
    Format.fprintf ppf "(set.inter %a %a)"
      r expr1 
      r expr2
  | BinOp (expr1, BVXor, expr2, _) -> 
    Format.fprintf ppf "(and (or %a %a) (not (and %a %a)))"
      r expr1 
      r expr2
      r expr1 
      r expr2
  | BinOp (expr1, op, expr2, _) -> 
    Format.fprintf ppf "(%a %a %a)"
      pp_print_binop op 
      r expr1 
      r expr2
  | CompOp (expr1, BVLt, expr2, _) -> 
    Format.fprintf ppf "(bvult %a %a)"
      r expr1 
      r expr2
  | CompOp (expr1, BVLte, expr2, _) -> 
    Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
      r expr1 
      r expr2
      r expr1 
      r expr2
  | CompOp (expr1, BVGt, expr2, _) -> 
    Format.fprintf ppf "(bvult %a %a)"
      r expr2
      r expr1 
  | CompOp (expr1, BVGte, expr2, _) -> 
    Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
      r expr2
      r expr1 
      r expr2
      r expr1 
  | CompOp (expr1, StrPrefix, expr2, _) -> 
    Format.fprintf ppf "(str.prefixof %a %a)"
      r expr1
      r expr2 
  | CompOp (expr1, StrContains, expr2, _) -> 
    Format.fprintf ppf "(str.contains %a %a)"
      r expr1
      r expr2 
  | CompOp (expr1, op, expr2, _) -> 
    Format.fprintf ppf "(%a %a %a)"
      pp_print_compop op 
      r expr1 
      r expr2
  | UnOp (op, expr, _) -> 
    Format.fprintf ppf "(%a %a)"
      pp_print_unop op 
      r expr
  | BuiltInFunc (StrLength, [expr], _) -> 
    Format.fprintf ppf "(str.len %a)"
      r expr
  | BuiltInFunc (SeqLength, [expr], _) -> 
    Format.fprintf ppf "(seq.len %a)"
      r expr
  | BuiltInFunc (Length, _, p) -> Utils.error "length(.) function is not yet supported in SMT constraints" p
  | BuiltInFunc (Repeat, _, p) -> Utils.error "repeat(.,.) function is not yet supported in SMT constraints" p
  | EmptySet (ty, _) -> 
    Format.fprintf ppf "(as set.empty (Set %a))"
      pp_print_ty ty  
  | Singleton (expr, _) -> 
    Format.fprintf ppf "(set.singleton %a)" 
      r expr
  | BVConst (_, bits, _) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "#b%a"
      (Lib.pp_print_list Format.pp_print_int "") bits
  | BConst (b, _) ->  Format.fprintf ppf "%b" b
  | IntConst (i, _) -> 
    if i >= 0 then 
      Format.fprintf ppf "%d" i
    else 
      Format.fprintf ppf "(- %d)" (i * -1)
  | StrConst (str, _) -> Format.fprintf ppf "\"%s\"" str
  | PhConst _ -> Utils.crash "Error: String constants can only be in dependencies (of the form 'nonterminal <- string_literal')"
  | BLConst _ -> Utils.crash "BitList literals not yet fully supported"
  | BVCast (width, e, _) -> 
    Format.fprintf ppf "((_ int_to_bv %d) %a)"
      width 
      r e 
  | BuiltInFunc (func, es, _) -> 
    Format.fprintf ppf "(%a %a)"
      A.pp_print_builtin_func func 
      (Lib.pp_print_list r " ") es
  | InhAttr _
  | SynthAttr _ -> assert false
