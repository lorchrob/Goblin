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

%token<bool list> BITS
%token<string> ID

%token EOF

%start <SygusAst.sygus_ast> s

%%

s: d = sygus_term; EOF { d } ;
	
sygus_term:
| LPAREN; LPAREN; DEFINE; HYPHEN; FUN; TOP; LPAREN; RPAREN; ID; t = lisp_term; RPAREN; RPAREN;
  { t }

lisp_term: 
| LPAREN; id = ID; ts = list(lisp_term); RPAREN; 
  { Node (id, ts) }
| bits = BITS; 
  { BVLeaf (List.length bits, bits) }
| id = ID; 
  { VarLeaf (id) }
| bits = bit_list; 
  { BLLeaf bits }

bit_list:
| LPAREN; AS; SEQ; DOT; EMPTY; LPAREN; CAPSEQ; BOOL; RPAREN; RPAREN; 
  { [] } 
| LPAREN; SEQ; DOT; UNIT; b = bool; RPAREN; 
  { [b] }
| LPAREN; SEQ; DOT; PLUSPLUS; bls = nonempty_list(bit_list); RPAREN;
  { List.flatten bls }


bool:
| TRUE { true }
| FALSE { false }