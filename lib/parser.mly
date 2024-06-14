%{
open Ast
%}

%token BOOL
%token INT
%token BITVECTOR
%token BITLIST
%token MACHINEINT
%token CASE
%token OF
%token TO
%token LENGTH
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
%token ARROW
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
%token UNDERSCORE

%token<int> INTEGER
%token<bool list> BITS
%token<string> ID

%token EOF

(* Priorities and associativity of operators, lowest first *)
%nonassoc OPTION
%nonassoc ARROW
%right LIMPLIES
%left BVOR BVXOR LOR LXOR
%left LAND BVAND
%left GT LTE EQ GTE LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc LNOT
%nonassoc BVNOT 
// %left DOT 
// %left SEMICOLON

%start <Ast.ast> s

%%

s: d = separated_list(SEMICOLON, element); EOF { d } ;
	
element:
(* Type annotaion *)
| nt = nonterminal; TYPEANNOT; t = il_type; 
  scs = option(semantic_constraints);
  { 
    match scs with 
    | None -> TypeAnnotation (nt, t, []) 
    | Some scs -> TypeAnnotation (nt, t, scs) 
  }
(* Production rule *)
| nt = nonterminal; PRODUCTION; ges = nonempty_list(grammar_element); 
  scs = option(semantic_constraints);
  { 
    match scs with 
    | None -> ProdRule (nt, ges, [])
    | Some scs -> ProdRule (nt, ges, scs) 
  }

il_type: 
| BOOL { Bool }
| INT { Int }
| BITVECTOR; LPAREN; len = INTEGER; RPAREN; { BitVector (len) }
| BITLIST { BitList }
| MACHINEINT; LPAREN; width = INTEGER; RPAREN; { MachineInt (width) }

semantic_constraints:
| LCURLY; scs = semantic_constraint_list; RCURLY; { scs }

semantic_constraint_list:
| sc = semantic_constraint { [sc] }
| sc = semantic_constraint; SEMICOLON; scs = semantic_constraint_list; { sc :: scs }

grammar_element:
| nt = nonterminal { Nonterminal(nt) }
| i = ID; EQ; nt = nonterminal; { NamedNonterminal (i, nt) }

semantic_constraint:
| nt = nonterminal; ASSIGN; e = expr { Dependency (nt, e) }
| e = expr { SyGuSExpr e }

expr: 
(* Binary operations *)
| e1 = expr; LAND; e2 = expr { Binop (e1, LAnd, e2) }
| e1 = expr; LOR; e2 = expr { Binop (e1, LOr, e2) }
| e1 = expr; LXOR; e2 = expr { Binop (e1, LXor, e2) }
| e1 = expr; LIMPLIES; e2 = expr { Binop (e1, LImplies, e2) }
| e1 = expr; PLUS; e2 = expr { Binop (e1, Plus, e2) }
| e1 = expr; MINUS; e2 = expr { Binop (e1, Minus, e2) }
| e1 = expr; TIMES; e2 = expr { Binop (e1, Times, e2) }
| e1 = expr; DIV; e2 = expr { Binop (e1, Div, e2) }
| e1 = expr; BVAND; e2 = expr { Binop (e1, BVAnd, e2) }
| e1 = expr; BVOR; e2 = expr { Binop (e1, BVOr, e2) }
| e1 = expr; BVXOR; e2 = expr { Binop (e1, BVXor, e2) }
(* Comparison operations *)
| e1 = expr; LT; e2 = expr { CompOp (e1, Lt, e2) }
| e1 = expr; LTE; e2 = expr { CompOp (e1, Lte, e2) }
| e1 = expr; GT; e2 = expr { CompOp (e1, Gt, e2) }
| e1 = expr; GTE; e2 = expr { CompOp (e1, Gte, e2) }
| e1 = expr; EQ; e2 = expr { CompOp (e1, Eq, e2) }
(* Unary operations *)
| BVNOT; e = expr { Unop (BVNot, e) }
| PLUS; e = expr { Unop (UPlus, e) }
| MINUS; e = expr { Unop (UMinus, e) }
| LNOT; e = expr { Unop (LNot, e) }
(* Concrete constants *)
| i = INTEGER; { IntConst (i) }
| TRUE; { BConst (true) }
| FALSE; { BConst (false) }
| bl = BITS { BLConst (bl) }
| LPAREN; BITLIST; bv = BITS; RPAREN; { BVConst (List.length bv, bv) }
(* Built-in functions *)
| INT; UNDERSCORE; TO; UNDERSCORE; BITVECTOR; 
  LPAREN; width = INTEGER; COMMA; e = INTEGER; RPAREN; { BVCast (width, e) }
| LENGTH; LPAREN; e = expr; RPAREN; { Length (e) }
(* Case expressions *)
| CASE; e = nt_expr; OF; cs = case_list { CaseExpr (e, cs) }
(* Variables *)
| e = nt_expr; index = option(index); RPAREN; { NTExpr (e, index) }
(* Arbitrary parens *)
| LPAREN; e = expr; RPAREN; { e }

case_list:
| OPTION; e1 = nt_expr; ARROW; e2 = expr; { [(e1, e2)] }
| OPTION; e1 = nt_expr; ARROW; e2 = expr; cs = case_list { (e1, e2) :: cs }

index:
| LPAREN; index = INTEGER; RPAREN; { index }

nt_expr: 
| nt = nonterminal { [nt] }
| nt = nonterminal; DOT; nts = nt_expr; { nt :: nts }

nonterminal:
| LT; str = ID; GT; { str }