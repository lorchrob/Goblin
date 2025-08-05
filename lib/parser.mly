%{
open Ast
%}

%token BOOL
%token INT
%token STRINGTYPE
%token BITVECTOR
%token INTTOBITVECTOR
%token BITLIST
// %token CASE
// %token OF
%token LENGTH
%token STRLENGTH
%token LAND
%token LOR
%token LXOR
%token LNOT
%token SET
%token LIST
%token BVAND
%token BVOR
%token BVXOR
%token BVNOT
%token PRODUCTION
%token OPTION
%token LT
%token GT
%token LTE
%token GTE
%token EQ
%token LIMPLIES
%token LCURLY
%token RCURLY
%token ASSIGN
// %token ARROW
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token RPAREN
%token LPAREN 
%token TYPEANNOT
%token COMMA
%token SEMICOLON
%token TRUE 
%token FALSE
%token DOT
%token BVLT
%token BVLTE
%token BVGT
%token BVGTE
%token STRPREFIX 
%token STRCONTAINS 
%token STRCONCAT
%token EMPTYSET
%token SINGLETON
%token UNION
%token INTERSECTION
%token MEMBER

%token STR_TO_RE ; 
%token STR_IN_RE ; 
%token RE_RANGE ; 
%token RE_UNION ; 
%token RE_STAR ; 
%token RE_CONCAT ;

%token<int> INTEGER
%token<bool list> BITS
%token<string> ID
%token<string> STRING

%token EOF

(* Priorities and associativity of operators, lowest first *)
// %nonassoc OPTION
// %nonassoc ARROW
// %left SEMICOLON

%start <Ast.ast> s
// %parameter <Lexing.position * Lexing.position>

%%

s: d = list(element); EOF { d } ;
	
element:
(* Type annotaion *)
| nt = nonterminal; TYPEANNOT; t = il_type; 
  scs = option(semantic_constraints); SEMICOLON;
  { 
    match scs with 
    | None -> TypeAnnotation (nt, t, []) 
    | Some scs -> TypeAnnotation (nt, t, scs) 
  }
(* Production rule *)
| nt = nonterminal; PRODUCTION; rhss = separated_nonempty_list(OPTION, rhs); SEMICOLON;
  { ProdRule (nt, rhss) }

rhs:
| ges = nonempty_list(grammar_element); scs = option(semantic_constraints);
  { 
    match scs with 
    | None -> Rhs (ges, [])
    | Some scs -> Rhs (ges, scs) 
  }

il_type: 
| BOOL { Bool }
| INT { Int }
| STRINGTYPE { String }
| BITVECTOR; LPAREN; len = INTEGER; RPAREN; { BitVector (len) }
| LIST; LPAREN; BOOL; RPAREN; { BitList }
| SET; LPAREN; ty = il_type; RPAREN; { Set ty } 

semantic_constraints:
| LCURLY; scs = semantic_constraint_list; RCURLY; { scs }

semantic_constraint_list:
| sc = semantic_constraint SEMICOLON; { [sc] }
| sc = semantic_constraint; SEMICOLON; scs = semantic_constraint_list; { sc :: scs }

grammar_element:
| nt = nonterminal { Nonterminal(nt, None) }

semantic_constraint:
| nt = nonterminal; ASSIGN; e = expr { Dependency (nt, e) }
| e = expr { SyGuSExpr e }

expr: 
| EMPTYSET; LT; ty = il_type; GT; 
  { EmptySet ty } 
| SINGLETON; LPAREN; e = expr; RPAREN;
  { Singleton e }
(* Inlined binop *)
| LPAREN; LAND; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, LAnd, e2) }
| LPAREN; LOR; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, LOr, e2) }
| LPAREN; LXOR; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, LXor, e2) }
| LPAREN; LIMPLIES; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, LImplies, e2) }
| LPAREN; PLUS; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, Plus, e2) }
| LPAREN; MINUS; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, Minus, e2) }
| LPAREN; TIMES; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, Times, e2) }
| LPAREN; DIV; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, Div, e2) }
| LPAREN; BVAND; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, BVAnd, e2) }
| LPAREN; BVOR; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, BVOr, e2) }
| LPAREN; BVXOR; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, BVXor, e2) }
| LPAREN; STRCONCAT; e1 = expr; e2 = expr; RPAREN; { BinOp (e1, StrConcat, e2) }
(* Inlined compop *)
| LPAREN; LT; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, Lt, e2) }
| LPAREN; LTE; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, Lte, e2) }
| LPAREN; GT; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, Gt, e2) }
| LPAREN; GTE; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, Gte, e2) }
| LPAREN; EQ; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, Eq, e2) }
| LPAREN; BVLT; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, BVLt, e2) }
| LPAREN; BVLTE; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, BVLte, e2) }
| LPAREN; BVGT; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, BVGt, e2) }
| LPAREN; BVGTE; e1 = expr; e2 = expr; RPAREN; { CompOp (e1, BVGte, e2) }
(* Inlined unop *)
| LPAREN; BVNOT; e = expr; RPAREN; { UnOp (BVNot, e) }
| LPAREN; PLUS; e = expr; RPAREN; { UnOp (UPlus, e) }
| LPAREN; MINUS; e = expr; RPAREN; { UnOp (UMinus, e) }
| LPAREN; LNOT; e = expr; RPAREN; { UnOp (LNot, e) }
(* Concrete constants *)
| i = INTEGER; { IntConst (i) }
| s = STRING; { StrConst (s) } 
| TRUE; { BConst true }
| FALSE; { BConst false }
| bv = BITS { BVConst (List.length bv, bv) }
| LPAREN; BITLIST; bl = BITS; RPAREN; { BLConst bl }
(* Built-in functions *)
| MEMBER; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { BinOp (e1, SetMembership, e2) }
| UNION; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { BinOp (e1, SetUnion, e2) }
| INTERSECTION; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { BinOp (e1, SetIntersection, e2) }
| INTTOBITVECTOR; 
  LPAREN; width = INTEGER; COMMA; e = expr; RPAREN; { BVCast (width, e) }
| LENGTH; LPAREN; e = expr; RPAREN; { Length (e) }
| STRLENGTH; LPAREN; e = expr; RPAREN; { StrLength (e) }
| STRPREFIX; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { CompOp (e1, StrPrefix, e2) }
| STRCONTAINS; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { CompOp (e1, StrContains, e2) }
| STR_TO_RE; LPAREN; e = expr; RPAREN; { StrToRe e }
| STR_IN_RE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { StrInRe (e1, e2) }
| RE_UNION; LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN; { ReUnion es } 
| RE_RANGE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { ReRange (e1, e2) } 
| RE_CONCAT; LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN; { ReConcat es }
| RE_STAR; LPAREN; e = expr; RPAREN; { ReStar e } 

(* Variables *)
| e = nt_expr; (* _ = option(index); *) { NTExpr ([], e) }
(* Arbitrary parens *)
| LPAREN; e = expr; RPAREN; { e }

nt_expr: 
| nt = nonterminal { [nt, None] }
| nt = nonterminal; DOT; nts = nt_expr; { (nt, None) :: nts }

nonterminal:
| LT; str = ID; GT; { str }
