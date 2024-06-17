type nt_expr = string list
type unary_operator = UPlus | UMinus | LNot | BVNot
type comp_operator = Lt | Lte | Gt | Gte | Eq
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
  | BVCast of int * int
  | CaseExpr of nt_expr * case list
  | NTExpr of nt_expr * int option
  | BVConst of int * bool list
  | BLConst of bool list
  | BConst of bool
  | IntConst of int
type semantic_constraint =
    Dependency of string * expr
  | SyGuSExpr of expr
type il_type = Bool | Int | BitVector of int | BitList | MachineInt of int
type grammar_element =
    Nonterminal of string
  | NamedNonterminal of string * string
type element =
    ProdRule of string * grammar_element list * semantic_constraint list
  | TypeAnnotation of string * il_type * semantic_constraint list
type ast = element list

val pp_print_ast : Format.formatter -> ast -> unit
val pp_print_nt_expr : Format.formatter -> nt_expr -> unit
