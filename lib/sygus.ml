open Ast

(* Module state for creating fresh identifiers *)
let i = ref 0

module TC = TypeChecker

let fresh_destructor: unit -> string 
= fun () ->
  let id = "des" ^ string_of_int !i in 
  i := !i + 1;
  id

let fresh_constraint: unit -> string 
= fun () ->
  let id = "c" ^ string_of_int !i in 
  i := !i + 1;
  id

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Int -> Format.fprintf ppf "Int"
| Bool -> Format.fprintf ppf "Bool"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| MachineInt width -> Format.fprintf ppf "(_ BitVec %d)" (Lib.pow 2 width)

let pp_print_constructor: TC.context -> Format.formatter -> grammar_element -> unit 
= fun ctx ppf ge -> match ge with 
| Nonterminal nt 
| NamedNonterminal (_, nt) -> 
  let d_str = fresh_destructor () in 
  let ty_str = match TC.StringMap.find_opt nt ctx with 
  | None -> String.uppercase_ascii nt
  | Some ty -> Utils.capture_output pp_print_ty ty
  in
  Format.fprintf ppf "(%s %s)"
  d_str
  ty_str

let pp_print_datatypes: Format.formatter -> TC.context -> ast -> unit 
= fun ppf ctx ast -> 
  List.iter (fun element -> match element with 
  | TypeAnnotation _ -> ()
  | ProdRule (nt, ges, _) -> 
    Format.fprintf ppf "(declare-datatype %s (\n\t(%s %a)\n))"
    (String.uppercase_ascii nt)
    ((String.lowercase_ascii nt) ^ "_con")
    (Lib.pp_print_list (pp_print_constructor ctx) " ") ges;
    Lib.print_newline ppf;
  ) ast 

let pp_print_nt_decs: Format.formatter -> ast -> unit 
= fun ppf ast -> List.iter (fun element -> match element with 
| ProdRule (nt, _, _) -> 
  Format.fprintf ppf "\t(%s %s)\n"
  (String.lowercase_ascii nt)
  (String.uppercase_ascii nt) 
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a)\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
) ast

let pp_print_binop: Format.formatter -> bin_operator -> unit 
= fun ppf op -> match op with 
| BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> failwith "BitVector xor is not supported"
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies ->Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"

let pp_print_compop: Format.formatter -> comp_operator -> unit 
= fun ppf op -> match op with 
| Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="

let pp_print_unop: Format.formatter -> unary_operator -> unit 
= fun ppf op -> match op with 
| UPlus -> ()
| UMinus -> Format.fprintf ppf "-"
| LNot -> Format.fprintf ppf "not"
| BVNot -> Format.fprintf ppf "bvnot"

let rec pp_print_expr: Format.formatter -> expr -> unit 
= fun ppf expr -> match expr with 
| NTExpr ([nt], None) -> Format.pp_print_string ppf (String.lowercase_ascii nt)
| BinOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
  pp_print_binop op 
  pp_print_expr expr1 
  pp_print_expr expr2
| CompOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
  pp_print_compop op 
  pp_print_expr expr1 
  pp_print_expr expr2
| UnOp (op, expr) -> 
  Format.fprintf ppf "(%a %a)"
  pp_print_unop op 
  pp_print_expr expr
| Length expr -> 
  Format.fprintf ppf "(seq.len %a)"
  pp_print_expr expr
| BVConst (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "#b%a"
  (Lib.pp_print_list Format.pp_print_int "") bits
| BConst b ->  Format.fprintf ppf "%b" b
| IntConst i -> Format.fprintf ppf "%d" i
| CaseExpr _  -> failwith "Case expressions not yet fully supported"
| NTExpr _ -> failwith "Nonterminal expressions with either dot notation or indexing are not yet fully supported" 
| BLConst _ -> failwith "BitList literals not yet fully supported"
| BVCast _ -> failwith "Internal error: a BV cast survived preprocessing!"

 
let pp_print_semantic_constraint: Format.formatter -> string -> grammar_element list -> semantic_constraint -> unit 
= fun ppf nt ges sc -> match sc with 
| Dependency _ -> () 
| SyGuSExpr expr -> 
  let constraint_id = fresh_constraint () in 
  let 
    ges = List.map Utils.grammar_element_to_string ges |> List.map String.lowercase_ascii 
  in 
  Format.fprintf ppf "(define-fun %s ((%s %s)) Bool \n\t(match %s (\n\t\t((%s %a)\n\t\t %a) \n\t))\n)"
  constraint_id
  (String.lowercase_ascii nt) 
  (String.uppercase_ascii nt) 
  (String.lowercase_ascii nt) 
  ((String.lowercase_ascii nt) ^ "_con") 
  (Lib.pp_print_list Format.pp_print_string " ") ges
  pp_print_expr expr; 
  Lib.print_newline ppf;
  Format.fprintf ppf "(constraint (%s top))"
  constraint_id

let pp_print_constraints: Format.formatter -> ast -> unit 
= fun ppf ast -> match ast with 
| [] -> failwith "Input grammar must have at least one production rule or type annotation"
| ProdRule (nt, ges, scs) :: _ -> 
  List.iter (pp_print_semantic_constraint ppf nt ges) scs
| TypeAnnotation _ :: _ -> 
  failwith "Semantic constraint with type annotation not yet supported"


  (* (sae_packet SAE_PACKET ((R3 auth_algo status_code)))
      (auth_algo AUTH_ALGO ((R2 bitvec_16)))
      (status_code STATUS_CODE ((R1 bitvec_16)))
      (bitvec_16 (_ BitVec 16) ((Constant (_ BitVec 16)))) *)
let pp_print_rules: Format.formatter -> ast -> unit 
= fun ppf ast -> List.iter (fun element -> match element with 
| ProdRule (nt, ges, _) -> 
  let ges = List.map Utils.grammar_element_to_string ges in 
  let ges = List.map String.lowercase_ascii ges in
  Format.fprintf ppf "\t(%s %s ((%s %a)))\n"
  (String.lowercase_ascii nt) 
  (String.uppercase_ascii nt) 
  ((String.lowercase_ascii nt) ^ "_con")
  (Lib.pp_print_list Format.pp_print_string " ") ges
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a ((Constant %a)))\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
  pp_print_ty ty
) ast

let pp_print_grammar: Format.formatter -> ast -> unit 
= fun ppf ast -> 
  let top_datatype_str = match List.hd ast with 
  | ProdRule (nt, _, _) 
  | TypeAnnotation (nt, _, _) -> String.uppercase_ascii nt
  in
  Format.fprintf ppf 
  "(synth-fun top () %s\n; declare nonterminals\n(\n%a)\n; grammar rules\n(\n%a)\n)"
   top_datatype_str
   pp_print_nt_decs ast 
   pp_print_rules ast

let pp_print_ast: Format.formatter -> TC.context -> ast -> unit 
= fun ppf ctx ast -> 
  Format.fprintf ppf "(set-logic ALL)";

  Lib.print_newline ppf;
  Lib.print_newline ppf;

  pp_print_datatypes ppf ctx ast;

  Lib.print_newline ppf;

  pp_print_grammar ppf ast;
  
  Lib.print_newline ppf;
  Lib.print_newline ppf;

  pp_print_constraints ppf ast;

  Lib.print_newline ppf;
  Lib.print_newline ppf;

  Format.fprintf ppf "(check-synth)";

  Lib.print_newline ppf;