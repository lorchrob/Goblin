%{
open Ast
%}

%token BOOL
%token INT
%token PLACEHOLDER
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
%token STRCONCAT

%token<int> INTEGER
%token<bool list> BITS
%token<string> ID
%token<string> STRING

%token EOF

(* Priorities and associativity of operators, lowest first *)
// %nonassoc OPTION
// %nonassoc ARROW
%right LIMPLIES
%left BVOR BVXOR LOR LXOR
%left LAND BVAND
%left GT LTE EQ GTE LT BVGT BVLTE BVGTE BVLT 
%left PLUS MINUS STRCONCAT
%left TIMES DIV
%nonassoc LNOT
%nonassoc BVNOT 
// %left DOT 
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
| PLACEHOLDER { Placeholder }
| STRINGTYPE { String }
| BITVECTOR; LPAREN; len = INTEGER; RPAREN; { BitVector (len) }
| BITLIST { BitList }

semantic_constraints:
| LCURLY; scs = semantic_constraint_list; RCURLY; { scs }

semantic_constraint_list:
| sc = semantic_constraint SEMICOLON; { [sc] }
| sc = semantic_constraint; SEMICOLON; scs = semantic_constraint_list; { sc :: scs }

grammar_element:
| nt = nonterminal { Nonterminal(nt) }

semantic_constraint:
| nt = nonterminal; ASSIGN; e = expr { Dependency (nt, e) }
| e = expr { SyGuSExpr e }

expr: 
(* Binary operations *)
| e1 = expr; LAND; e2 = expr { BinOp (e1, LAnd, e2) }
| e1 = expr; LOR; e2 = expr { BinOp (e1, LOr, e2) }
| e1 = expr; LXOR; e2 = expr { BinOp (e1, LXor, e2) }
| e1 = expr; LIMPLIES; e2 = expr { BinOp (e1, LImplies, e2) }
| e1 = expr; PLUS; e2 = expr { BinOp (e1, Plus, e2) }
| e1 = expr; MINUS; e2 = expr { BinOp (e1, Minus, e2) }
| e1 = expr; TIMES; e2 = expr { BinOp (e1, Times, e2) }
| e1 = expr; DIV; e2 = expr { BinOp (e1, Div, e2) }
| e1 = expr; BVAND; e2 = expr { BinOp (e1, BVAnd, e2) }
| e1 = expr; BVOR; e2 = expr { BinOp (e1, BVOr, e2) }
| e1 = expr; BVXOR; e2 = expr { BinOp (e1, BVXor, e2) }
| e1 = expr; STRCONCAT; e2 = expr { BinOp (e1, StrConcat, e2) }
(* Comparison operations *)
| e1 = expr; LT; e2 = expr { CompOp (e1, Lt, e2) }
| e1 = expr; LTE; e2 = expr { CompOp (e1, Lte, e2) }
| e1 = expr; GT; e2 = expr { CompOp (e1, Gt, e2) }
| e1 = expr; GTE; e2 = expr { CompOp (e1, Gte, e2) }
| e1 = expr; EQ; e2 = expr { CompOp (e1, Eq, e2) }
| e1 = expr; BVLT; e2 = expr { CompOp (e1, BVLt, e2) }
| e1 = expr; BVLTE; e2 = expr { CompOp (e1, BVLte, e2) }
| e1 = expr; BVGT; e2 = expr { CompOp (e1, BVGt, e2) }
| e1 = expr; BVGTE; e2 = expr { CompOp (e1, BVGte, e2) }
(* Unary operations *)
| BVNOT; e = expr { UnOp (BVNot, e) }
| PLUS; e = expr { UnOp (UPlus, e) }
| MINUS; e = expr { UnOp (UMinus, e) }
| LNOT; e = expr { UnOp (LNot, e) }
(* Concrete constants *)
| i = INTEGER; { IntConst (i) }
| s = STRING; { PhConst (s) }
| TRUE; { BConst (true) }
| FALSE; { BConst (false) }
| bv = BITS { BVConst (List.length bv, bv) }
| LPAREN; BITLIST; bl = BITS; RPAREN; { BLConst (bl) }
(* Built-in functions *)
| INTTOBITVECTOR; 
  LPAREN; width = INTEGER; COMMA; e = expr; RPAREN; { BVCast (width, e) }
| LENGTH; LPAREN; e = expr; RPAREN; { Length (e) }
| STRLENGTH; LPAREN; e = expr; RPAREN; { StrLength (e) }
| STRPREFIX; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { CompOp (e1, StrPrefix, e2) }
(* Case expressions *)
// | CASE; e = nt_expr; OF; cs = case_list { Match (e, cs) }
(* Variables *)
| e = nt_expr; (* _ = option(index); *) { NTExpr ([], e) }
(* Arbitrary parens *)
| LPAREN; e = expr; RPAREN; { e }

// case_list:
// | OPTION; e1 = nt_expr; ARROW; e2 = expr; { [(e1, e2)] }
// | OPTION; e1 = nt_expr; ARROW; e2 = expr; cs = case_list { (e1, e2) :: cs }

// index:
// | LPAREN; index = INTEGER; RPAREN; { index }

nt_expr: 
| nt = nonterminal { [nt, None] }
| nt = nonterminal; DOT; nts = nt_expr; { (nt, None) :: nts }

nonterminal:
| LT; str = ID; GT; { str }