%{
open SolverAst
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
%token UNIT_TYPE
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
%token AT
%token<string> STRCONST

%token<bool list> BITS
%token<string> ID
%token<int> INTEGER

%token EOF

%start <SolverAst.solver_ast> s

%%

s: 
| d = term; EOF { d } 
| SAT; model = model; EOF { model } 
| model = model; EOF { model } 
| UNSAT; EOF { Infeasible }

model: 
| LPAREN; values = list(model_value); RPAREN; { Model values }

model_value:
| LPAREN; DEFINEFUN; id = ID; LPAREN; RPAREN; UNIT_TYPE; 
    LPAREN; AS; AT; ID; UNIT_TYPE; RPAREN;
  RPAREN; 
 { (id, Value.Unit) }
| LPAREN; DEFINEFUN; id = ID; LPAREN; RPAREN; il_ty; v = value; RPAREN;
 { (id, v) }
	
term:
| LPAREN; LPAREN; DEFINEFUN; TOP; LPAREN; RPAREN; top_type; t = lisp_term; RPAREN; RPAREN;
  { t }
| INFEASIBLE;
  { Infeasible }

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

(* SyGuS-style term *)
lisp_term: 
| LPAREN; id = ID; ts = list(lisp_term); RPAREN; 
  { Node ((Nt.User id, None, None), ts) }
| id = ID; 
  { Leaf (Placeholder id) }
| v = value; 
  { Leaf v }

value:
| bits = BITS; 
  { Value.BitVector (List.length bits, bits) }
| bits = bit_list; 
  { Value.BitList bits }
| ss = string_set; 
  { Value.StringSet ss }
| i = INTEGER; 
  { Value.Int i }
| LPAREN; HYPHEN; i = INTEGER; RPAREN; 
  { Value.Int (-i) }
| TRUE; 
  { Value.Bool true }
| FALSE; 
  { Value.Bool false }
| str = STRCONST;
  { Value.String str }

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
