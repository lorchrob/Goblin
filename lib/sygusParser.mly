%{
open SygusAst
%}

%token DEFINE
%token FUN 
%token TOP
%token HYPHEN
%token LPAREN 
%token RPAREN
%token AS
%token SEQ 
%token CAPSEQ
%token DOT
%token EMPTY
%token BOOL
%token UNIT
%token TRUE
%token FALSE
%token PLUSPLUS
%token INT
%token UNDERSCORE
%token BITVEC
%token STR
%token STRING
%token INFEASIBLE
%token UNSAT
%token<string> STRCONST

%token<bool list> BITS
%token<string> ID
%token<int> INTEGER

%token EOF

%start <SygusAst.sygus_ast> s

%%

s: 
| d = sygus_term; EOF { d } 
| model = sygus_model; EOF { model } 
| UNSAT; EOF { VarLeaf "infeasible" }

sygus_model: 
| LPAREN; values = list(model_value); RPAREN; { Node ("smt_model", values) }

model_value:
| LPAREN; DEFINE; HYPHEN; FUN; id = ID; LPAREN; RPAREN; il_ty; t = lisp_term; RPAREN; { Node (id, [t]) }
	
sygus_term:
| LPAREN; LPAREN; DEFINE; HYPHEN; FUN; TOP; LPAREN; RPAREN; top_type; t = lisp_term; RPAREN; RPAREN;
  { t }
| INFEASIBLE;
  { VarLeaf "infeasible" }

top_type:
| ID; {}
| il_ty; {}

il_ty:
| INT; {}
| BOOL; {}
| STRING; {}
| LPAREN; CAPSEQ; BOOL; RPAREN; {}
| LPAREN; UNDERSCORE; BITVEC; INTEGER; RPAREN; {}

lisp_term: 
| LPAREN; id = ID; ts = list(lisp_term); RPAREN; 
  { Node (id, ts) }
| bits = BITS; 
  { BVLeaf (List.length bits, bits) }
| id = ID; 
  { VarLeaf id }
| bits = bit_list; 
  { BLLeaf bits }
| i = INTEGER; 
  { IntLeaf i }
| LPAREN; HYPHEN; i = INTEGER; RPAREN; 
  { IntLeaf (-i) }
| str = STRCONST;
  { StrLeaf str }

bit_list:
| LPAREN; AS; SEQ; DOT; EMPTY; LPAREN; CAPSEQ; BOOL; RPAREN; RPAREN; 
  { [] } 
| LPAREN; SEQ; DOT; UNIT; b = bool; RPAREN; 
  { [b] }
| LPAREN; STR; DOT; PLUSPLUS; bls = nonempty_list(bit_list); RPAREN;
  { List.flatten bls }
| LPAREN; SEQ; DOT; PLUSPLUS; bls = nonempty_list(bit_list); RPAREN;
{ List.flatten bls }


bool:
| TRUE { true }
| FALSE { false }