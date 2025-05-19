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
| StrPrefix

type bin_operator =
| BVAnd
| BVOr
| BVXor
| LAnd
(* Generated logical and, to distinguish from user input logical and *)
| GLAnd
| LOr
| LXor
| LImplies
| Plus
| Minus
| Times
| Div
| StrConcat

type case = 
(* A case is a list of <context, nonterminal> pairs (denoting a pattern) and the corresponding expression *)
(* The int option eases dealing with horizontal ambiguous references *)
| Case of ((string * int option) list * (string * int option)) list * expr 
| CaseStub of ((string * int option) list * (string * int option)) list
and expr =
| BinOp of expr * bin_operator * expr
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr
| Length of expr
| StrLength of expr
| BVCast of int * expr
(* First string list track the context of the nonterminal being matched 
   Int options are for clarifying ambiguous dot notation references, as in NTExpr *)
| Match of (string * int option) list * (string * int option) * case list
(* First string list tracks the context of a nonterminal after desugaring to match expression
   Second int list is for dot notation input e.g. <A>.<B>.<C> 
   Int option is for disambiguating references. 
   
   More detail:
   First string list is initially empty. When we desugar, e.g., dot expression 
   <A>.<B> to a match expression match <A> with | ... <B> ... -> <expr containing <B>>
   the expression containing <B> needs to remember it came from <A>, in case of name 
   clashes. So, this dot notation context is stored in the first string list.

   The int options are initially None, but may be populated by the tool as a  
   structured form of renaming to clarify ambiguous dot notation references.
   *)
| NTExpr of (string * int option) list * (string * int option) list
| BVConst of int * bool list
| BLConst of bool list
| BConst of bool
| IntConst of int
| PhConst of string
| StrConst of string

type semantic_constraint =
| Dependency of string * expr
| SyGuSExpr of expr

type il_type = Bool | Int | Placeholder | String | BitVector of int | BitList | ADT of string list list

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

val get_nts_from_expr : expr -> string list
val get_nts_from_sc: semantic_constraint -> string list 
val get_nts_from_expr_after_desugaring_dot_notation : expr -> (string * int option) list list 
val pp_print_element: Format.formatter -> element ->  unit 
val pp_print_ast : Format.formatter -> ast -> unit
val pp_print_nt_with_dots : Format.formatter -> (string * int option) list -> unit
val pp_print_nt_with_underscores : Format.formatter -> (string * int option) list -> unit
val pp_print_expr : Format.formatter -> expr -> unit
val pp_print_ty : Format.formatter -> il_type -> unit
val pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit
val pp_print_prod_rule_rhs: Format.formatter -> prod_rule_rhs -> unit
val il_int_to_bitvector : int -> int -> expr
val grammar_element_to_string : grammar_element -> string
val nts_of_rhs: prod_rule_rhs -> string list 
val expr_contains_dangling_nt: Utils.SILSet.t -> expr -> bool 
val ast_constrains_nt: ast -> string -> bool
val prepend_nt_to_dot_exprs: string -> expr -> expr
val get_nts_from_expr2: expr -> (string * int option) list list