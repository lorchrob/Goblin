type nonterminal = string
type identifier = nonterminal
type nt_expr = identifier list
type unary_operator = UPlus | UMinus | LNot | BVNot
type comp_operator = Lt | Lte | Gt | Gte | Eq
type bin_operator =
    BVAnd
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
    Binop of expr * bin_operator * expr
  | Unop of unary_operator * expr
  | CompOp of expr * comp_operator * expr
  | Length of expr
  | BVCast of int * int
  | CaseExpr of nt_expr * case list
  | NTExpr of nt_expr * int option
  | Identifier of identifier
  | BVConst of int * bool list
  | BLConst of bool list
  | BConst of bool
  | IntConst of int
type semantic_constraint =
    Dependency of identifier * expr
  | SyGuSExpr of expr
type il_type = Bool | Int | BitVector of int | BitList | MachineInt of int
type grammar_element =
    Nonterminal of identifier
  | NamedNonterminal of identifier * identifier
type element =
    ProdRule of identifier * grammar_element list * semantic_constraint list
  | TypeAnnotation of identifier * il_type * semantic_constraint list
type ast = element list
