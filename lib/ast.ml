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
| StrPrefix

type bin_operator = 
| BVAnd 
| BVOr 
| BVXor 
| LAnd
(* Generated logical and, to distinguish from user input logical and *)
| GLAnd
| LOr
| LXor 
| LImplies 
| Plus 
| Minus 
| Times 
| Div
| StrConcat

type case = 
(* A case is a list of <context, nonterminal> pairs (denoting a pattern) and the corresponding expression *)
(* The int option eases dealing with horizontal ambiguous references *)
| Case of ((string * int option) list * (string * int option)) list * expr 
| CaseStub of ((string * int option) list * (string * int option)) list
and
expr = 
| BinOp of expr * bin_operator * expr 
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr 
| Length of expr
| StrLength of expr
| BVCast of int * expr
(* First string list track the context of the nonterminal being matched 
   Int options are for clarifying ambiguous dot notation references, as in NTExpr *)
| Match of (string * int option) list * (string * int option) * case list
(* First string list tracks the context of a nonterminal after desugaring to match expression
   Second int list is for dot notation input e.g. <A>.<B>.<C> 
   Int option is for disambiguating references. 
   
   More detail:
   First string list is initially empty. When we desugar, e.g., dot expression 
   <A>.<B> to a match expression match <A> with | ... <B> ... -> <expr containing <B>>
   the expression containing <B> needs to remember it came from <A>, in case of name 
   clashes. So, this dot notation context is stored in the first string list.

   The int options are initially None, but may be populated by the tool as a  
   structured form of renaming to clarify ambiguous dot notation references.
   *)
| NTExpr of (string * int option) list * (string * int option) list
| BVConst of int * bool list
| BLConst of bool list
| BConst of bool
| IntConst of int 
| PhConst of string
| StrConst of string

type semantic_constraint = 
| Dependency of string * expr (* <nonterminal> <- <expression> *)
| SyGuSExpr of expr (* Any boolean expression *)

type il_type = 
| Bool 
| Int 
| Placeholder
| String
| BitVector of int 
| BitList
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

(* This is the type of the grammar terms *)
type ast = element list

(* This function is used before desugaring dot expressions *)
(* TODO: Why is this function returning a string list rather than a string list list? *)
let rec get_nts_from_expr: expr -> string list 
= fun expr -> 
  let r = get_nts_from_expr in
  match expr with 
  | NTExpr (_, nts) -> List.map fst nts 
  | Match (_, (nt, _), cases) -> nt :: (List.map (fun case -> match case with 
    | CaseStub _ -> []
    | Case (_, expr) -> r expr
    ) cases |> List.flatten)
  | BinOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | StrLength expr
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | PhConst _
  | IntConst _ 
  | StrConst _ -> []

(* This function is used before desugaring dot expressions *)
let rec get_nts_from_expr2: expr -> (string * int option) list list
= fun expr -> 
  let r = get_nts_from_expr2 in
  match expr with 
  | NTExpr (nts1, nts2) -> [nts1 @ nts2]
  | Match _ -> Utils.crash "Unexpected case in get_nts_from_expr2"
  | BinOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | StrLength expr
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | PhConst _
  | IntConst _ 
  | StrConst _ -> []

(* For when you want to process simple NTs after translation of dot to match expressions *)
let rec get_nts_from_expr_after_desugaring_dot_notation: expr -> (string * int option) list list 
= fun expr -> 
  let r = get_nts_from_expr_after_desugaring_dot_notation in
  match expr with 
  | NTExpr (nt_ctx, (nt, idx) :: _) -> [nt_ctx @ [nt, idx]] 
  | NTExpr _ -> Utils.crash "Impossible case in get_nts_from_expr_after_desugaring_dot_notation"
  | Match (nt_ctx, (nt, idx), cases) -> (nt_ctx @ [nt, idx]) :: 
    (List.map (fun case -> match case with 
    | CaseStub pattern -> List.map (fun (nt_ctx, (nt, idx)) -> nt_ctx @ [nt, idx]) pattern
    | Case (pattern, expr) -> 
      List.map (fun (nt_ctx, (nt, idx)) -> nt_ctx @ [nt, idx]) pattern @ r expr
    ) cases |> List.flatten)
  | BinOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 @ r expr2
  | StrLength expr
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | PhConst _
  | IntConst _ 
  | StrConst _ -> []

let pp_print_nt_helper_dots: Format.formatter -> string * int option -> unit 
= fun ppf (nt, idx) -> 
  Format.fprintf ppf "<%s%s>" 
    nt
    (match idx with 
    | None -> ""
    | Some i -> string_of_int i)

let pp_print_nt_helper_underscores: Format.formatter -> string * int option -> unit 
= fun ppf (nt, idx) -> 
  Format.fprintf ppf "%s%s" 
    nt
    (match idx with 
    | None -> ""
    | Some i -> string_of_int i)

let pp_print_nt_with_dots: Format.formatter -> (string * int option) list -> unit
= fun ppf nt_expr -> 
  Format.fprintf ppf "%a"
  (Lib.pp_print_list pp_print_nt_helper_dots ".") nt_expr 

let pp_print_nt_with_underscores: Format.formatter -> (string * int option) list -> unit
= fun ppf nt_expr -> 
  Format.fprintf ppf "<%a>"
  (Lib.pp_print_list pp_print_nt_helper_underscores "_") nt_expr 

let pp_print_bin_op: Format.formatter -> bin_operator -> unit
= fun ppf op -> match op with 
| BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> Format.fprintf ppf "bvxor"
| GLAnd 
| LAnd -> Format.fprintf ppf "land"
| LOr -> Format.fprintf ppf "lor"
| LXor -> Format.fprintf ppf "lxor"
| LImplies -> Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"
| StrConcat -> Format.fprintf ppf ("str.++")

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
| StrPrefix -> Format.fprintf ppf "str.prefixof"

(* let pp_print_bit: Format.formatter -> bool ->  *)

let pp_print_pattern: Format.formatter -> ((string * int option) list * (string * int option)) -> unit
= fun ppf (nt_ctx, (nt, idx)) -> 
    pp_print_nt_with_underscores ppf (nt_ctx @ [nt, idx])

let rec pp_print_case: Format.formatter -> case -> unit 
= fun ppf case -> 
  match case with 
  | Case (pattern, expr) -> 
    Format.fprintf ppf "| %a -> %a"
      (Lib.pp_print_list pp_print_pattern " ") pattern 
      pp_print_expr expr
  | CaseStub (pattern) -> 
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
| StrLength expr -> 
  Format.fprintf ppf "str_length(%a)"
    pp_print_expr expr 
| Length expr -> 
  Format.fprintf ppf "length(%a)"
    pp_print_expr expr 
| BVCast (width, expr) -> 
  Format.fprintf ppf "int_to_bitvector(%d, %a)" 
    width 
    pp_print_expr expr 
| Match (nts, nt, cases) -> 
  Format.fprintf ppf "(match %a with %a)"
    pp_print_nt_with_underscores (nts @ [nt])
    (Lib.pp_print_list pp_print_case " ") cases 
| NTExpr (nts, [nt_expr]) -> pp_print_nt_with_underscores ppf (nts @ [nt_expr]) 
| NTExpr ([], nt_expr) -> pp_print_nt_with_dots ppf (nt_expr) 
| NTExpr (nt_ctx, nts) -> 
  Format.fprintf ppf "%a:%a"
    pp_print_nt_with_underscores nt_ctx 
    pp_print_nt_with_dots nts
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
| PhConst s -> Format.fprintf ppf "%S" s
| StrConst s -> Format.fprintf ppf "%s" s

let pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit 
= fun ppf sc -> match sc with 
| Dependency (nt, expr) -> 
  Format.fprintf ppf "<%a> <- %a;"
    Format.pp_print_string nt 
    pp_print_expr expr
| SyGuSExpr expr -> 
  Format.fprintf ppf "%a;"
    pp_print_expr expr

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Bool -> Format.fprintf ppf "Bool"
| Int -> Format.fprintf ppf "Int"
| Placeholder -> Format.fprintf ppf "Placeholder"
| String -> Format.fprintf ppf "String"
| BitList -> Format.fprintf ppf "BitList" 
| BitVector width -> Format.fprintf ppf "BitVector(%d)" width
| ADT rules -> 
  Format.fprintf ppf "ADT: %a"
    (Lib.pp_print_list (Lib.pp_print_list Format.pp_print_string " ") "; ") rules

let pp_print_grammar_element: Format.formatter -> grammar_element ->  unit 
= fun ppf g_el -> match g_el with 
| Nonterminal nt -> pp_print_nt_with_dots ppf [nt, None]
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
    pp_print_nt_with_dots [nt, None]
    (Lib.pp_print_list pp_print_prod_rule_rhs " | ") rhss

| TypeAnnotation (nt, ty, []) -> 
  Format.fprintf ppf "%a :: %a;"
    pp_print_nt_with_dots [nt, None] 
    pp_print_ty ty

| TypeAnnotation (nt, ty, scs) -> 
  Format.fprintf ppf "%a :: %a { %a };"
    pp_print_nt_with_dots [nt, None] 
    pp_print_ty ty
    (Lib.pp_print_list pp_print_semantic_constraint " ") scs

let pp_print_ast: Format.formatter -> ast ->  unit 
= fun ppf ast -> 
  Format.fprintf ppf "%a\n"
    (Lib.pp_print_list pp_print_element "\n") ast

let il_int_to_bitvector: int -> int -> expr 
= fun length n ->
  if n >= (1 lsl length) then
    (* NOTE: If we overflow, return max value *)
    BVConst (length, Utils.replicate true length)
  else
    let rec to_bits acc len n =
      if len = 0 then acc
      else
        let bit = (n land 1) = 1 in
        to_bits (bit :: acc) (len - 1) (n lsr 1)
    in
    let bits = to_bits [] length n in 
    BVConst (length, bits)

let grammar_element_to_string: grammar_element -> string 
= fun grammar_element -> match grammar_element with 
  | Nonterminal nt2 -> nt2
  | StubbedNonterminal (_, stub_id) -> stub_id

(* Used before divide and conquer *)
let nts_of_rhs: prod_rule_rhs -> string list 
= fun rhs -> match rhs with 
| Rhs (ges, _) -> 
  List.map (fun ge -> match ge with 
  | Nonterminal nt -> nt 
  | StubbedNonterminal _ -> assert false
  ) ges
| StubbedRhs _ -> assert false

let rec expr_contains_dangling_nt: Utils.SILSet.t -> expr -> bool 
= fun ctx expr -> 
  let r = expr_contains_dangling_nt ctx in
  match expr with 
  | NTExpr (nt_ctx, nts) -> 
    let nt_expr = Utils.init (nt_ctx @ nts) in  
    (List.length nt_expr > 1) && 
    not (Utils.SILSet.mem nt_expr ctx)
  | BinOp (expr1, _, expr2) -> 
    r expr1 || r expr2
  | UnOp (_, expr) -> 
    r expr
  | CompOp (expr1, _, expr2) -> 
    r expr1 || r expr2
  | StrLength expr
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | PhConst _
  | IntConst _ 
  | StrConst _ -> false
  | Match _ -> 
    Utils.crash "Encountered Match in expr_contains_dangling_nt. 
    Shouldn't be possible, as this function should only process base expressions."

let sc_constrains_nt: string -> semantic_constraint -> bool 
= fun nt sc -> match sc with 
| SyGuSExpr expr -> List.mem nt (get_nts_from_expr expr)
| Dependency (nt2, _) -> nt = nt2

let get_nts_from_sc: semantic_constraint -> string list 
= fun sc -> match sc with 
| SyGuSExpr expr -> get_nts_from_expr expr
| Dependency (nt2, _) -> [nt2]

(* To be called before desugaring NTs to match expressions and resolving ambiguities *)
let ast_constrains_nt: ast -> string -> bool 
= fun ast nt -> 
  List.exists (fun element -> match element with 
    | TypeAnnotation (nt2, _, _ :: _) when nt = nt2 -> true 
    | TypeAnnotation _ -> false
    | ProdRule (_, rhss) -> List.exists (fun rhs -> match rhs with 
      | Rhs (_, scs) -> List.exists (sc_constrains_nt nt) scs
      | StubbedRhs _ -> false
    ) rhss
  ) ast

(* Use before desugaring NTs to match expressions*)
let rec prepend_nt_to_dot_exprs: string -> expr -> expr 
= fun nt expr -> 
  let r = prepend_nt_to_dot_exprs nt in
  match expr with
  | NTExpr ([], nts) -> NTExpr ([], (nt, None) :: nts)
  | BVCast (len, expr) -> BVCast (len, r expr)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2) 
  | UnOp (op, expr) -> UnOp (op, r expr) 
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2) 
  | StrLength expr -> StrLength (r expr)
  | Length expr -> Length (r expr) 
  | Match _ -> Utils.crash "Unexpected case 1 in prepend_nt_to_dot_exprs"
  | NTExpr _ -> Utils.crash "Unexpected case 2 in prepend_nt_to_dot_exprs" 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr
