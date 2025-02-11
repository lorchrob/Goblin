type unary_operator = UPlus | UMinus | LNot | BVNot

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

type case = 
| Case of string list * expr 
| CaseStub of string list
and expr =
| BinOp of expr * bin_operator * expr
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr
| Length of expr
| BVCast of int * expr
| Match of string * case list
| NTExpr of string list
| BVConst of int * bool list
| BLConst of bool list
| BConst of bool
| IntConst of int
| StrConst of string

type semantic_constraint =
| Dependency of string * expr
| SyGuSExpr of expr

type il_type = Bool | Int | String | BitVector of int | BitList | MachineInt of int | ADT of string list list

type grammar_element =
| Nonterminal of string
| StubbedNonterminal of string * string

type prod_rule_rhs = 
| Rhs of grammar_element list * semantic_constraint list
| StubbedRhs of string

type element =
| ProdRule of string * prod_rule_rhs list
| TypeAnnotation of string * il_type * semantic_constraint list

type ast = element list

val rename : expr -> (string * string) list -> expr 
val get_nts_from_expr : expr -> string list
val get_nts_from_expr_shallow : expr -> string list
val pp_print_element: Format.formatter -> element ->  unit 
val pp_print_ast : Format.formatter -> ast -> unit
val pp_print_nt_expr : Format.formatter -> string list -> unit
val pp_print_expr : Format.formatter -> expr -> unit
val pp_print_ty : Format.formatter -> il_type -> unit
val pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit