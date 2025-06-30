%{
open SygusAst
%}

%token HYPHEN
%token DEFINEFUN
%token TOP
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
%token STRINGTYPE
%token INFEASIBLE
%token UNSAT
%token SAT
%token SET
%token SETTYPE
%token UNION
%token SINGLETON
%token<string> STRCONST

%token<bool list> BITS
%token<string> ID
%token<int> INTEGER

%token EOF

%start <SygusAst.sygus_ast> s

%%

s: 
| d = sygus_term; EOF { d } 
| SAT; model = sygus_model; EOF { model } 
| model = sygus_model; EOF { model } 
| UNSAT; EOF { VarLeaf "infeasible" }

sygus_model: 
| LPAREN; values = list(model_value); RPAREN; { Node (("smt_model", None), values) }

model_value:
| LPAREN; DEFINEFUN; id = ID; LPAREN; RPAREN; il_ty; t = lisp_term; RPAREN;
 { let id, idx = Utils.parse_str_nat_suffix id in
  Node ((id, idx), [t]) }
	
sygus_term:
| LPAREN; LPAREN; DEFINEFUN; TOP; LPAREN; RPAREN; top_type; t = lisp_term; RPAREN; RPAREN;
  { t }
| INFEASIBLE;
  { VarLeaf "infeasible" }

top_type:
| ID; {}
| il_ty; {}

il_ty:
| INT; {}
| BOOL; {}
| STRINGTYPE; {}
| LPAREN; CAPSEQ; BOOL; RPAREN; {}
| LPAREN; UNDERSCORE; BITVEC; INTEGER; RPAREN; {}
| LPAREN; SETTYPE; il_ty; RPAREN; {}

lisp_term: 
| LPAREN; id = ID; ts = list(lisp_term); RPAREN; 
  { let id, idx = Utils.parse_str_nat_suffix id in
    Node ((id, idx), ts) }
| bits = BITS; 
  { BVLeaf (List.length bits, bits) }
| id = ID; 
  { VarLeaf id }
| bits = bit_list; 
  { BLLeaf bits }
| ss = string_set; 
  { SetLeaf (StringSet ss) }
| i = INTEGER; 
  { IntLeaf i }
| LPAREN; HYPHEN; i = INTEGER; RPAREN; 
  { IntLeaf (-i) }
| TRUE; 
  { BoolLeaf true }
| FALSE; 
  { BoolLeaf false }
| str = STRCONST;
  { StrLeaf str }

string_set:
| LPAREN; AS; SET; DOT; EMPTY; LPAREN; SETTYPE; STRINGTYPE; RPAREN; RPAREN; 
  { Utils.StringSet.empty }
| LPAREN; SET; DOT; SINGLETON; s = STRCONST; RPAREN;
  { Utils.StringSet.singleton s }
| LPAREN; SET; DOT; UNION; sss = nonempty_list(string_set); RPAREN;
  { List.fold_left Utils.StringSet.union Utils.StringSet.empty sss }

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
