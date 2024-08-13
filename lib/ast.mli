type nt_expr = string list

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

type case = nt_expr * expr
and expr =
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
| StrConst of string

type semantic_constraint =
| Dependency of string * expr
| SyGuSExpr of expr

type il_type = Bool | Int | String | BitVector of int | BitList | MachineInt of int

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

type ast = element list

val substitute : expr -> string -> expr -> expr 

val pp_print_element: Format.formatter -> element ->  unit 
val pp_print_ast : Format.formatter -> ast -> unit
val pp_print_nt_expr : Format.formatter -> nt_expr -> unit
val pp_print_expr : Format.formatter -> expr -> unit
val pp_print_ty : Format.formatter -> il_type -> unit