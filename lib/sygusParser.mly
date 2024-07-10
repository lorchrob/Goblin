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

%token<bool list> BITS
%token<string> ID
%token<int> INTEGER

%token EOF

%start <SygusAst.sygus_ast> s

%%

s: d = sygus_term; EOF { d } ;
	
sygus_term:
| LPAREN; LPAREN; DEFINE; HYPHEN; FUN; TOP; LPAREN; RPAREN; top_type; t = lisp_term; RPAREN; RPAREN;
  { t }

top_type:
| ID; {}
| il_ty; {}

il_ty:
| INT; {}
| BOOL; {}
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

bit_list:
| LPAREN; AS; SEQ; DOT; EMPTY; LPAREN; CAPSEQ; BOOL; RPAREN; RPAREN; 
  { [] } 
| LPAREN; SEQ; DOT; UNIT; b = bool; RPAREN; 
  { [b] }
| LPAREN; STR; DOT; PLUSPLUS; bls = nonempty_list(bit_list); RPAREN;
  { List.flatten bls }


bool:
| TRUE { true }
| FALSE { false }