type nt_expr = string list

type unary_operator = 
| UPlus 
| UMinus
| LNot
| BVNot

type comp_operator = 
| Lt 
| Lte 
| Gt 
| Gte 
| Eq
| BVLt 
| BVLte
| BVGt 
| BVGte

type bin_operator = 
| BVAnd 
| BVOr 
| BVXor 
| LAnd
| LOr
| LXor 
| LImplies 
| Plus 
| Minus 
| Times 
| Div

type case = nt_expr * expr 
and
expr = 
| BinOp of expr * bin_operator * expr 
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr 
| Length of expr
| BVCast of int * expr
| CaseExpr of nt_expr * case list
| NTExpr of nt_expr * int option 
| BVConst of int * bool list
| BLConst of bool list
| BConst of bool
| IntConst of int 

type semantic_constraint = 
| Dependency of string * expr (* <nonterminal> <- <expression> *)
| SyGuSExpr of expr (* Any boolean expression *)

type il_type = 
| Bool 
| Int 
| BitVector of int 
| BitList
| MachineInt of int

type grammar_element = 
| Nonterminal of string 
| NamedNonterminal of string * string
| StubbedNonterminal of string * string

type prod_rule_rhs = 
| Rhs of grammar_element list * semantic_constraint list
| StubbedRhs of string

type element = 
| ProdRule of string * prod_rule_rhs list
| TypeAnnotation of string * il_type * semantic_constraint list

(* DANIYAL: This is the type of the grammar terms *)
type ast = element list

(* 
Basic ADT:
type btree = 
| Leaf of int
| Node of btree * int * btree 
*)

(* Substitute e1 for var in e2. In other words, output e1[var\e2] *)
let rec substitute: expr -> string -> expr -> expr 
= fun e1 var e2 -> 
  match e1 with 
  | NTExpr ([id], None) -> 
    if id = var then e2 else e1 
  | NTExpr _ -> failwith "Nested or indexed NTExprs not yet supported"
  | CaseExpr _ -> failwith "CaseExpr not yet supported"
  | BinOp (expr1, op, expr2) -> BinOp (substitute expr1 var e2, op, substitute expr2 var e2) 
  | UnOp (op, expr) -> UnOp (op, substitute expr var e2) 
  | CompOp (expr1, op, expr2) -> CompOp (substitute expr1 var e2, op, substitute expr2 var e2) 
  | Length expr -> Length (substitute expr var e2) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | IntConst _ -> e1


let pp_print_nonterminal: Format.formatter -> string -> unit 
= fun ppf nt -> 
  Format.fprintf ppf "<%s>" nt

let pp_print_nt_expr: Format.formatter -> nt_expr -> unit
= fun ppf nt_expr -> 
  Lib.pp_print_list pp_print_nonterminal "." ppf nt_expr 

let pp_print_bin_op: Format.formatter -> bin_operator -> unit
= fun ppf op -> match op with 
| BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> Format.fprintf ppf "bvxor"
| LAnd -> Format.fprintf ppf "land"
| LOr -> Format.fprintf ppf "lor"
| LXor -> Format.fprintf ppf "lxor"
| LImplies -> Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"

let pp_print_unary_op: Format.formatter -> unary_operator -> unit 
= fun ppf op -> match op with 
| UPlus -> Format.fprintf ppf  "+"
| UMinus -> Format.fprintf ppf  "-"
| LNot -> Format.fprintf ppf  "lnot"
| BVNot -> Format.fprintf ppf  "bvnot"

let pp_print_comp_op: Format.formatter -> comp_operator -> unit 
= fun ppf op -> match op with 
| Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt  -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="
| BVLt -> Format.fprintf ppf  "bvlt"
| BVLte -> Format.fprintf ppf  "bvlte"
| BVGt -> Format.fprintf ppf  "bvgt"
| BVGte -> Format.fprintf ppf  "bvgte"

(* let pp_print_bit: Format.formatter -> bool ->  *)

let rec pp_print_case: Format.formatter -> case -> unit 
= fun ppf (nt_expr, expr) -> 
  Format.fprintf ppf "| %a -> %a"
    pp_print_nt_expr nt_expr 
    pp_print_expr expr

and pp_print_expr: Format.formatter -> expr -> unit 
= fun ppf expr -> match expr with
| BinOp (expr1, op, expr2) -> 
  Format.fprintf ppf "%a %a %a" 
    pp_print_expr expr1 
    pp_print_bin_op op 
    pp_print_expr expr2
| UnOp (op, expr) -> 
  Format.fprintf ppf "%a %a" 
    pp_print_unary_op op 
    pp_print_expr expr
| CompOp (expr1, op, expr2) -> 
  Format.fprintf ppf "%a %a %a" 
    pp_print_expr expr1 
    pp_print_comp_op op 
    pp_print_expr expr2
| Length expr -> 
  Format.fprintf ppf "length(%a)"
    pp_print_expr expr 
| BVCast (width, expr) -> 
  Format.fprintf ppf "int_to_bitvector(%d, %a)" 
    width 
    pp_print_expr expr 
| CaseExpr (nt, cases) -> 
  Format.fprintf ppf "case %a of %a"
    pp_print_nt_expr nt 
    (Lib.pp_print_list pp_print_case " ") cases 
| NTExpr (nt_expr, None) -> pp_print_nt_expr ppf nt_expr 
| NTExpr (nt_expr, Some index) -> 
  Format.fprintf ppf "%a(%d)"
    pp_print_nt_expr nt_expr 
    index
| BLConst bits -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "(BitList 0b%a)"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BVConst (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BConst b -> Format.fprintf ppf "%b" b
| IntConst i -> Format.fprintf ppf "%d" i

let pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit 
= fun ppf sc -> match sc with 
| Dependency (nt, expr) -> 
  Format.fprintf ppf "%a <- %a;"
    pp_print_nonterminal nt 
    pp_print_expr expr
| SyGuSExpr expr -> 
  Format.fprintf ppf "%a;"
    pp_print_expr expr

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Bool -> Format.fprintf ppf "Bool"
| Int -> Format.fprintf ppf "Int"
| BitList -> Format.fprintf ppf "BitList" 
| BitVector width -> Format.fprintf ppf "BitVector(%d)" width
| MachineInt width -> Format.fprintf ppf "MachineInt(%d)" width

let pp_print_grammar_element: Format.formatter -> grammar_element ->  unit 
= fun ppf g_el -> match g_el with 
| Nonterminal nt -> pp_print_nonterminal ppf nt
| NamedNonterminal (id, nt) -> 
  Format.fprintf ppf "%s = %a" 
    id 
    pp_print_nonterminal nt
| StubbedNonterminal (_, stub_id) -> Format.pp_print_string ppf stub_id

let pp_print_prod_rule_rhs: Format.formatter -> prod_rule_rhs -> unit 
= fun ppf rhss -> 
  match rhss with 
| Rhs (ges, []) -> 
  Format.fprintf ppf "%a"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
| Rhs (ges, scs) ->
  Format.fprintf ppf "%a \n{ %a }"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
  (Lib.pp_print_list pp_print_semantic_constraint " ") scs
| StubbedRhs stub_id -> 
  Format.pp_print_string ppf stub_id

let pp_print_element: Format.formatter -> element ->  unit 
= fun ppf el -> match el with 
| ProdRule (nt, rhss) -> 
  Format.fprintf ppf "%a ::= %a;"
    pp_print_nonterminal nt
    (Lib.pp_print_list pp_print_prod_rule_rhs " | ") rhss

| TypeAnnotation (nt, ty, []) -> 
  Format.fprintf ppf "%a :: %a;"
    pp_print_nonterminal nt 
    pp_print_ty ty

| TypeAnnotation (nt, ty, scs) -> 
  Format.fprintf ppf "%a :: %a { %a };"
    pp_print_nonterminal nt 
    pp_print_ty ty
    (Lib.pp_print_list pp_print_semantic_constraint " ") scs

let pp_print_ast: Format.formatter -> ast ->  unit 
= fun ppf ast -> 
  Format.fprintf ppf "%a\n"
    (Lib.pp_print_list pp_print_element "\n") ast