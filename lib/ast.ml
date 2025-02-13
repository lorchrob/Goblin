type unary_operator = 
| UPlus 
| UMinus
| LNot
| BVNot

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

type bin_operator = 
| BVAnd 
| BVOr 
| BVXor 
| LAnd
| LOr
| LXor 
| LImplies 
| Plus 
| Minus 
| Times 
| Div

type case = 
(* A case is a list of <context, nonterminal> pairs (denoting a pattern) and the corresponding expression *)
| Case of (string list * string) list * expr 
| CaseStub of (string list * string) list
and
expr = 
| BinOp of expr * bin_operator * expr 
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr 
| Length of expr
| BVCast of int * expr
(* First string list track the context of the nonterminal being matched *)
| Match of string list * string * case list
(* First string list tracks the context of a nonterminal after desugaring to match expression
   Second int list is for dot notation input e.g. <A>.<B>.<C> 
   Int option is for disambiguating references. 
   
   More detail:
   First string list is initially empty. When we desugar, e.g., dot expression 
   <A>.<B> to a match expression match <A> with | ... <B> ... -> <expr containing <B>>
   the expression containing <B> needs to remember it came from <A>, in case of name 
   clashes. So, this dot notation context is stored in the first string list.
   *)
| NTExpr of string list * string list * int option
| BVConst of int * bool list
| BLConst of bool list
| BConst of bool
| IntConst of int 
| StrConst of string

type semantic_constraint = 
| Dependency of string * expr (* <nonterminal> <- <expression> *)
| SyGuSExpr of expr (* Any boolean expression *)

type il_type = 
| Bool 
| Int 
| String
| BitVector of int 
| BitList
| MachineInt of int
| ADT of string list list

type grammar_element = 
| Nonterminal of string 
| StubbedNonterminal of string * string (* Ignore *)

type prod_rule_rhs = 
| Rhs of grammar_element list * semantic_constraint list
| StubbedRhs of string (* Ignore *)

type element = 
| ProdRule of string * prod_rule_rhs list
| TypeAnnotation of string * il_type * semantic_constraint list

(* DANIYAL: This is the type of the grammar terms *)
type ast = element list

(* 
Basic ADT:
type btree = 
| Leaf of int
| Node of btree * int * btree 
*)

(* Substitute e1 for var in e2. In other words, output e1[var\e2] *)
(* let rec rename: expr -> (string * string) list -> expr 
= fun e renaming -> 
  match e with 
  | NTExpr ([id]) -> (
    match List.assoc_opt id renaming with 
    | Some id2 -> NTExpr ([id2])
    | None -> e
    )
  | NTExpr _ -> failwith "Nested or indexed NTExprs not yet supported"
  | Match (nt, cases) -> (
    match List.assoc_opt nt renaming with 
    | Some nt2 -> Match (nt2, cases)
    | None -> e
    )
  | BinOp (expr1, op, expr2) -> BinOp (rename expr1 renaming, op, rename expr2 renaming) 
  | UnOp (op, expr) -> UnOp (op, rename expr renaming) 
  | CompOp (expr1, op, expr2) -> CompOp (rename expr1 renaming, op, rename expr2 renaming) 
  | Length expr -> Length (rename expr renaming) 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> e *)

(* This function is used before desugaring dot expressions *)
let rec get_nts_from_expr: expr -> string list 
= fun expr -> 
  let r = get_nts_from_expr in
  match expr with 
  | NTExpr (_, nts, _) -> nts 
  | Match (_, nt, cases) -> [nt] @ (List.map (fun case -> match case with 
    | CaseStub _ -> []
    | Case (_, expr) -> r expr
    ) cases |> List.flatten)
  | BinOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> []

(* For when you want to process simple NTs after translation of dot to match expressions *)
(* let rec get_nts_from_expr_shallow: expr -> string list 
= fun expr -> 
  let r = get_nts_from_expr_shallow in
  match expr with 
  | NTExpr (nt :: _) -> [nt] 
  | NTExpr _ -> failwith "Impossible case in get_nts_from_expr_shallow"
  | Match (nt, _) -> [nt]
  | BinOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> [] *)

let pp_print_nonterminal: Format.formatter -> string -> unit 
= fun ppf nt -> 
  Format.fprintf ppf "<%s>" nt

let pp_print_nt_expr: Format.formatter -> string list -> unit
= fun ppf nt_expr -> 
  Lib.pp_print_list pp_print_nonterminal "." ppf nt_expr 

let pp_print_bin_op: Format.formatter -> bin_operator -> unit
= fun ppf op -> match op with 
| BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> Format.fprintf ppf "bvxor"
| LAnd -> Format.fprintf ppf "land"
| LOr -> Format.fprintf ppf "lor"
| LXor -> Format.fprintf ppf "lxor"
| LImplies -> Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"

let pp_print_unary_op: Format.formatter -> unary_operator -> unit 
= fun ppf op -> match op with 
| UPlus -> Format.fprintf ppf  "+"
| UMinus -> Format.fprintf ppf  "-"
| LNot -> Format.fprintf ppf  "lnot"
| BVNot -> Format.fprintf ppf  "bvnot"

let pp_print_comp_op: Format.formatter -> comp_operator -> unit 
= fun ppf op -> match op with 
| Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt  -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="
| BVLt -> Format.fprintf ppf  "bvlt"
| BVLte -> Format.fprintf ppf  "bvlte"
| BVGt -> Format.fprintf ppf  "bvgt"
| BVGte -> Format.fprintf ppf  "bvgte"

(* let pp_print_bit: Format.formatter -> bool ->  *)

let pp_print_pattern: Format.formatter -> (string list * string) -> unit
= fun ppf (nt_ctx, nt) -> 
  Format.fprintf ppf "<%a>"
    (Lib.pp_print_list Format.pp_print_string "_") (nt_ctx @ [nt])

let rec pp_print_case: Format.formatter -> case -> unit 
= fun ppf case -> 
  match case with 
  | Case (pattern, expr) -> 
    Format.fprintf ppf "| %a -> %a"
      (Lib.pp_print_list pp_print_pattern " ") pattern 
      pp_print_expr expr
  | CaseStub pattern -> 
    Format.fprintf ppf "| %a -> STUB"
      (Lib.pp_print_list pp_print_pattern " ") pattern

and pp_print_expr: Format.formatter -> expr -> unit 
= fun ppf expr -> match expr with
| BinOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)" 
    pp_print_expr expr1 
    pp_print_bin_op op 
    pp_print_expr expr2
| UnOp (op, expr) -> 
  Format.fprintf ppf "(%a %a)" 
    pp_print_unary_op op 
    pp_print_expr expr
| CompOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)" 
    pp_print_expr expr1 
    pp_print_comp_op op 
    pp_print_expr expr2
| Length expr -> 
  Format.fprintf ppf "length(%a)"
    pp_print_expr expr 
| BVCast (width, expr) -> 
  Format.fprintf ppf "int_to_bitvector(%d, %a)" 
    width 
    pp_print_expr expr 
| Match (nts, nt, cases) -> 
  Format.fprintf ppf "(match <%a> with %a)"
    (Lib.pp_print_list Format.pp_print_string "_") (nts @ [nt]) 
    (Lib.pp_print_list pp_print_case " ") cases 
| NTExpr ([], nt_expr, None) -> pp_print_nt_expr ppf nt_expr 
| NTExpr ([], nt_expr, Some i) -> 
    Format.fprintf ppf "%a(%d)"
      pp_print_nt_expr nt_expr 
      i 
| NTExpr (nts, nt_expr, None) -> 
  Format.fprintf ppf "<%a>"
    (Lib.pp_print_list Format.pp_print_string "_") (nts @ nt_expr) 
| NTExpr (nts, nt_expr, Some i) -> 
    Format.fprintf ppf "<%a(%d)>"
      (Lib.pp_print_list Format.pp_print_string "_") (nts @ nt_expr)
      i 
| BLConst bits -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "(BitList 0b%a)"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BVConst (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BConst b -> Format.fprintf ppf "%b" b
| IntConst i -> Format.fprintf ppf "%d" i
| StrConst s -> Format.fprintf ppf "\"%s\"" s

let pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit 
= fun ppf sc -> match sc with 
| Dependency (nt, expr) -> 
  Format.fprintf ppf "%a <- %a;"
    pp_print_nonterminal nt 
    pp_print_expr expr
| SyGuSExpr expr -> 
  Format.fprintf ppf "%a;"
    pp_print_expr expr

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Bool -> Format.fprintf ppf "Bool"
| Int -> Format.fprintf ppf "Int"
| String -> Format.fprintf ppf "String"
| BitList -> Format.fprintf ppf "BitList" 
| BitVector width -> Format.fprintf ppf "BitVector(%d)" width
| MachineInt width -> Format.fprintf ppf "MachineInt(%d)" width
| ADT rules -> 
  Format.fprintf ppf "ADT: %a"
    (Lib.pp_print_list (Lib.pp_print_list Format.pp_print_string " ") "; ") rules

let pp_print_grammar_element: Format.formatter -> grammar_element ->  unit 
= fun ppf g_el -> match g_el with 
| Nonterminal nt -> pp_print_nonterminal ppf nt
| StubbedNonterminal (_, stub_id) -> Format.pp_print_string ppf stub_id

let pp_print_prod_rule_rhs: Format.formatter -> prod_rule_rhs -> unit 
= fun ppf rhss -> 
  match rhss with 
| Rhs (ges, []) -> 
  Format.fprintf ppf "%a"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
| Rhs (ges, scs) ->
  Format.fprintf ppf "%a \n{ %a }"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
  (Lib.pp_print_list pp_print_semantic_constraint " ") scs
| StubbedRhs stub_id -> 
  Format.pp_print_string ppf stub_id

let pp_print_element: Format.formatter -> element ->  unit 
= fun ppf el -> match el with 
| ProdRule (nt, rhss) -> 
  Format.fprintf ppf "%a ::= %a;"
    pp_print_nonterminal nt
    (Lib.pp_print_list pp_print_prod_rule_rhs " | ") rhss

| TypeAnnotation (nt, ty, []) -> 
  Format.fprintf ppf "%a :: %a;"
    pp_print_nonterminal nt 
    pp_print_ty ty

| TypeAnnotation (nt, ty, scs) -> 
  Format.fprintf ppf "%a :: %a { %a };"
    pp_print_nonterminal nt 
    pp_print_ty ty
    (Lib.pp_print_list pp_print_semantic_constraint " ") scs

let pp_print_ast: Format.formatter -> ast ->  unit 
= fun ppf ast -> 
  Format.fprintf ppf "%a\n"
    (Lib.pp_print_list pp_print_element "\n") ast