%{
open Ast
%}

%token UNIT
%token BOOL
%token INT
%token STRINGTYPE
%token BITVECTOR
%token INTTOBITVECTOR
%token UBV_TO_INT
%token SBV_TO_INT
%token BITLIST
// %token CASE
// %token OF
%token LENGTH
%token STRLENGTH
%token SEQLENGTH
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
%token MOD
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
%token REPEAT ;
%token GETS ;

%token<int> INTEGER
%token<float> DECIMAL
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
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc LNOT
%nonassoc BVNOT 
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
    | None -> TypeAnnotation (nt, t, [], $startpos) 
    | Some scs -> TypeAnnotation (nt, t, scs, $startpos) 
  }
(* Attribute type annotation *) 
| attribute = ID; TYPEANNOT; t = il_type; SEMICOLON;
  { 
    TypeAnnotation ("%_" ^ attribute, t, [], $startpos) 
  }
(* Production rule *)
| nt = nonterminal; PRODUCTION; rhss = separated_nonempty_list(OPTION, rhs); SEMICOLON;
  { 
    ProdRule (nt, rhss, $startpos) 
  }

rhs:
| ges = nonempty_list(grammar_element); scs = option(semantic_constraints); prob = option(DECIMAL);
  { 
    match scs with 
    | None -> Rhs (ges, [], prob, $startpos)
    | Some scs -> Rhs (ges, scs, prob, $startpos) 
  }

il_type: 
| UNIT { Unit }
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
| nt = nonterminal { 
    Nonterminal(nt, None, $startpos) 
  }

semantic_constraint:
| nt = nonterminal; ASSIGN; e = expr { 
    DerivedField (nt, e, $startpos) 
  }
| e = expr { 
    SmtConstraint (e, $startpos) 
  }
| attr = ID; GETS; e = expr { 
    AttrDef (attr, e, $startpos)
  }

expr: 
| EMPTYSET; LT; ty = il_type; GT; 
  { 
    EmptySet (ty, $startpos) 
  } 
| SINGLETON; LPAREN; e = expr; RPAREN;
  { 
    Singleton (e, $startpos) 
  }
(* Binary operations *)
| e1 = expr; LAND; e2 = expr { 
    BinOp (e1, LAnd, e2, $startpos) 
  }
| e1 = expr; LOR; e2 = expr { 
    BinOp (e1, LOr, e2, $startpos) 
  }
| e1 = expr; LXOR; e2 = expr { 
    BinOp (e1, LXor, e2, $startpos) 
  }
| e1 = expr; LIMPLIES; e2 = expr { 
    BinOp (e1, LImplies, e2, $startpos) 
  }
| e1 = expr; PLUS; e2 = expr { 
    BinOp (e1, Plus, e2, $startpos) 
  }
| e1 = expr; MINUS; e2 = expr { 
    BinOp (e1, Minus, e2, $startpos) 
  }
| e1 = expr; TIMES; e2 = expr { 
    BinOp (e1, Times, e2, $startpos) 
  }
| e1 = expr; DIV; e2 = expr { 
    BinOp (e1, Div, e2, $startpos) 
  }
| e1 = expr; MOD; e2 = expr { 
    BinOp (e1, Mod, e2, $startpos) 
  }
| e1 = expr; BVAND; e2 = expr { 
    BinOp (e1, BVAnd, e2, $startpos) 
  }
| e1 = expr; BVOR; e2 = expr { 
    BinOp (e1, BVOr, e2, $startpos) 
  }
| e1 = expr; BVXOR; e2 = expr { 
    BinOp (e1, BVXor, e2, $startpos) 
  }
(* Comparison operations *)
| e1 = expr; LT; e2 = expr { 
    CompOp (e1, Lt, e2, $startpos) 
  }
| e1 = expr; LTE; e2 = expr { 
    CompOp (e1, Lte, e2, $startpos) 
  }
| e1 = expr; GT; e2 = expr { 
    CompOp (e1, Gt, e2, $startpos) 
  }
| e1 = expr; GTE; e2 = expr { 
    CompOp (e1, Gte, e2, $startpos) 
  }
| e1 = expr; EQ; e2 = expr { 
    CompOp (e1, Eq, e2, $startpos) 
  }
| e1 = expr; BVLT; e2 = expr { 
    CompOp (e1, BVLt, e2, $startpos) 
  }
| e1 = expr; BVLTE; e2 = expr { 
    CompOp (e1, BVLte, e2, $startpos) 
  }
| e1 = expr; BVGT; e2 = expr { 
    CompOp (e1, BVGt, e2, $startpos) 
  }
| e1 = expr; BVGTE; e2 = expr { 
    CompOp (e1, BVGte, e2, $startpos) 
  }
(* Unary operations *)
| BVNOT; e = expr { 
    UnOp (BVNot, e, $startpos) 
  }
| PLUS; e = expr { 
    UnOp (UPlus, e, $startpos) 
  }
| MINUS; e = expr { 
    UnOp (UMinus, e, $startpos) 
  }
| LNOT; e = expr { 
    UnOp (LNot, e, $startpos) 
  }
(* Concrete constants *)
| i = INTEGER; { 
    IntConst (i, $startpos) 
  }
| s = STRING; { 
    StrConst (s, $startpos) 
  } 
| TRUE; { 
    BConst (true, $startpos) 
  }
| FALSE; { 
    BConst (false, $startpos) 
  }
| bv = BITS { 
    BVConst (List.length bv, bv, $startpos) 
  }
| LPAREN; BITLIST; bl = BITS; RPAREN; { 
    BLConst (bl, $startpos) 
  }
(* Built-in functions *)
| STRCONCAT; LPAREN; e1 = expr; COMMA; e2 = expr RPAREN; { 
    BinOp (e1, StrConcat, e2, $startpos) 
  }
| MEMBER; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { 
    BinOp (e1, SetMembership, e2, $startpos) 
  }
| UNION; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { 
    BinOp (e1, SetUnion, e2, $startpos) 
  }
| INTERSECTION; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
  { 
    BinOp (e1, SetIntersection, e2, $startpos) 
  }
| INTTOBITVECTOR; 
  LPAREN; width = INTEGER; COMMA; e = expr; RPAREN; { 
    BVCast (width, e, $startpos) 
  }
| UBV_TO_INT; 
  LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (UbvToInt, [e], $startpos) 
  }
| SBV_TO_INT; 
  LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (SbvToInt, [e], $startpos) 
  }
| LENGTH; LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (Length, [e], $startpos) 
  }
| STRLENGTH; LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (StrLength, [e], $startpos) 
  }
| SEQLENGTH; LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (SeqLength, [e], $startpos) 
  }
| STRPREFIX; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { 
    CompOp (e1, StrPrefix, e2, $startpos) 
  }
| STRCONTAINS; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { 
    CompOp (e1, StrContains, e2, $startpos) 
  }
| STR_TO_RE; LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (StrToRe, [e], $startpos) 
  }
| STR_IN_RE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { 
    BuiltInFunc (StrInRe, [e1; e2], $startpos) 
  }
| RE_UNION; LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN; { 
    BuiltInFunc (ReUnion, es, $startpos) 
  } 
| RE_RANGE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { 
    BuiltInFunc (ReRange, [e1; e2], $startpos) 
  } 
| RE_CONCAT; LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN; { 
    BuiltInFunc (ReConcat, es, $startpos) 
  }
| RE_STAR; LPAREN; e = expr; RPAREN; { 
    BuiltInFunc (ReStar, [e], $startpos) 
  } 
| REPEAT; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { 
    BuiltInFunc (Repeat, [e1; e2], $startpos) 
  } 
| e = nt_expr; (* _ = option(index); *) { 
    NTExpr ([], e, $startpos) 
  }
| nt = nonterminal; DOT; attr = ID; { 
    SynthAttr (nt, attr, $startpos) 
  }
| LPAREN; e = expr; RPAREN; { e }

nt_expr: 
| nt = nonterminal { [nt, None] }
| nt = nonterminal; DOT; nts = nt_expr; { (nt, None) :: nts }

nonterminal:
| LT; str = ID; GT; { str }
