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
| StrContains

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
| SetUnion
| SetIntersection
| SetMembership

type il_type = 
| Unit
| Bool 
| Int 
| Placeholder 
| String 
| BitVector of int 
| BitList 
| ADT of string list list 
| Set of il_type

type case = 
(* A case is a list of <context, nonterminal> pairs (denoting a pattern) and the corresponding expression *)
(* The int option eases dealing with horizontal ambiguous references *)
| Case of ((string * int option) list * (string * int option)) list * expr 
| CaseStub of ((string * int option) list * (string * int option)) list
and expr =
| EmptySet of il_type * Lexing.position
| Singleton of expr * Lexing.position
| BinOp of expr * bin_operator * expr * Lexing.position
| UnOp of unary_operator * expr * Lexing.position
| CompOp of expr * comp_operator * expr * Lexing.position
| Length of expr * Lexing.position
| StrLength of expr * Lexing.position
| SeqLength of expr * Lexing.position
| BVCast of int * expr * Lexing.position
(* First string list track the context of the nonterminal being matched 
   Int options are for clarifying ambiguous dot notation references, as in NTExpr *)
| Match of (string * int option) list * (string * int option) * case list * Lexing.position
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
| NTExpr of (string * int option) list * (string * int option) list * Lexing.position
| BVConst of int * bool list * Lexing.position
| BLConst of bool list * Lexing.position
| BConst of bool * Lexing.position
| IntConst of int * Lexing.position
| PhConst of string * Lexing.position
| StrConst of string * Lexing.position
| ReUnion of expr list * Lexing.position
| ReRange of expr * expr * Lexing.position
| StrInRe of expr * expr * Lexing.position
| StrToRe of expr * Lexing.position
| ReStar of expr * Lexing.position
| ReConcat of expr list * Lexing.position

type semantic_constraint =
| DerivedField of string * expr * Lexing.position
| SmtConstraint of expr * Lexing.position

type grammar_element =
| Nonterminal of string * int option * Lexing.position
| StubbedNonterminal of string * string

type prod_rule_rhs = 
| Rhs of grammar_element list * semantic_constraint list * Lexing.position
| StubbedRhs of string

type element =
| ProdRule of string * prod_rule_rhs list * Lexing.position
| TypeAnnotation of string * il_type * semantic_constraint list * Lexing.position

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
val il_int_to_bv : int -> int -> Lexing.position -> expr
val grammar_element_to_string : grammar_element -> string
val nts_of_rhs: prod_rule_rhs -> string list 
val expr_contains_dangling_nt: Utils.SILSet.t -> expr -> bool 
val ast_constrains_nt: ast -> string -> bool
val prepend_nt_to_dot_exprs: string -> expr -> expr
val get_nts_from_expr2: expr -> (string * int option) list list
val scs_of_element: element -> semantic_constraint list
val nts_of_ast: ast -> Utils.StringSet.t 
