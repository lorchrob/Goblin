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
(* The int option eases dealing with horizontal ambiguous references *)
| Case of ((string * int option) list * (string * int option)) list * expr 
| CaseStub of ((string * int option) list * (string * int option)) list
and
expr = 
| BinOp of expr * bin_operator * expr 
| UnOp of unary_operator * expr
| CompOp of expr * comp_operator * expr 
| Length of expr
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


let rec drop lst n =
  match (lst, n) with
  | (xs, 0) -> xs
  | ([], x) -> 
    if x = 0 then [] else
    failwith "Internal error: drop"
  | (_ :: xs, n) -> drop xs (n - 1);;


let prefixes lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux ((match acc with [] -> [x] | p :: _ -> (p @ [x])) :: acc) xs
  in
  aux [] lst

(* Base case of rename (below). Instead of renaming when there is an exact match of 
   NTExprs, we rename for any matching prefix. *)
let rename_base_case nt_expr renaming = 
  let prefixes = prefixes nt_expr in 
  let renamed_prefix = List.fold_left (fun acc nt_expr -> 
    if acc != None then acc else
    match List.assoc_opt (nt_expr) renaming with 
    | Some i -> 
      let nt_ctx = Utils.init nt_expr in 
      let (id, _) = Utils.last nt_expr in
      Some (nt_ctx @ [id, Some i])
    | None -> None
  ) None prefixes in 
  match renamed_prefix with 
  | None -> nt_expr 
  | Some renamed_prefix -> 
    let prefix_len = List.length renamed_prefix in 
    renamed_prefix @ (drop nt_expr prefix_len)

(* Substitute e1 for var in e2. *)
(* TODO: 
  1. Update the renaming to apply to nt prefixes, not just if the entire nt is an exact
     match for the substitution
  2. If we are matching on an nt with N possible renamings (including the previous generalization),
     then we need to generate N nested matches on the nt (one for each renaming) BEFORE recursing 
    *)
let rec rename: expr -> ((string * int option) list * int) list -> expr 
= fun e renaming -> 
  match e with 
  | NTExpr (nt_ctx, [id, idx]) -> 
    let nt_expr = rename_base_case (nt_ctx @ [id, idx]) renaming in 
    NTExpr (Utils.init nt_expr, [Utils.last nt_expr])
  | Match (nt_ctx, (id, idx), cases) -> (
    let cases = List.map (fun case -> match case with 
    | CaseStub pattern -> 
      let pattern = List.map (fun (nt_ctx, (id, idx)) ->
        let nt_expr = rename_base_case (nt_ctx @ [id, idx]) renaming in 
        Utils.init nt_expr, Utils.last nt_expr
      ) pattern in
      CaseStub pattern
    | Case (pattern, expr) -> 
      let pattern = List.map (fun (nt_ctx, (id, idx)) ->
        let nt_expr = rename_base_case (nt_ctx @ [id, idx]) renaming in 
        Utils.init nt_expr, Utils.last nt_expr
      ) pattern in
      Case (pattern, rename expr renaming)
    ) cases in
    (* match List.assoc_opt (nt_ctx @ [id, idx]) renaming with 
    | Some i -> Match (nt_ctx, (id, Some i), cases)
    | None -> Match (nt_ctx, (id, idx), cases)  *)
    let nt_expr = rename_base_case (nt_ctx @ [id, idx]) renaming in 
    Match (Utils.init nt_expr, Utils.last nt_expr, cases)
    )
  | BinOp (expr1, op, expr2) -> BinOp (rename expr1 renaming, op, rename expr2 renaming) 
  | UnOp (op, expr) -> UnOp (op, rename expr renaming) 
  | CompOp (expr1, op, expr2) -> CompOp (rename expr1 renaming, op, rename expr2 renaming) 
  | Length expr -> Length (rename expr renaming) 
  | NTExpr _ -> failwith "Internal error in rename: case should be impossible"
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> e

(* This function is used before desugaring dot expressions *)
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
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> []

(* For when you want to process simple NTs after translation of dot to match expressions *)
let rec get_nts_from_expr_after_desugaring_dot_notation: expr -> (string * int option) list list 
= fun expr -> 
  let r = get_nts_from_expr_after_desugaring_dot_notation in
  match expr with 
  | NTExpr (nt_ctx, (nt, idx) :: _) -> [nt_ctx @ [nt, idx]] 
  | NTExpr _ -> failwith "Impossible case in get_nts_from_expr_after_desugaring_dot_notation"
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
  | Length expr -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _ -> []

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
| StrConst s -> Format.fprintf ppf "\"%s\"" s

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
| String -> Format.fprintf ppf "String"
| BitList -> Format.fprintf ppf "BitList" 
| BitVector width -> Format.fprintf ppf "BitVector(%d)" width
| MachineInt width -> Format.fprintf ppf "MachineInt(%d)" width
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