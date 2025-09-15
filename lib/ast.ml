(* TODO
* Support n-ary expressions
* make function names consistent with smt-lib
* nt[i] notation
* Don't use separate AST constructor for each built-in function; instead, use 
  BinaryFunc, NaryFunc, etc. parameterized by func_name 
  *)

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
| StrContains

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
| Mod
| StrConcat
| SetUnion
| SetIntersection
| SetMembership 

type il_type = 
| Unit
| Bool 
| Int 
| Placeholder
| String
| BitVector of int 
| BitList
| ADT of string list list
| Set of il_type

type case = 
(* A case is a list of <context, nonterminal> pairs (denoting a pattern) and the corresponding expression *)
(* The int option eases dealing with horizontal ambiguous references *)
| Case of ((string * int option) list * (string * int option)) list * expr 
| CaseStub of ((string * int option) list * (string * int option)) list
and
expr = 
| EmptySet of il_type * Lexing.position
| Singleton of expr * Lexing.position
| BinOp of expr * bin_operator * expr * Lexing.position
| UnOp of unary_operator * expr * Lexing.position
| CompOp of expr * comp_operator * expr * Lexing.position
| Length of expr * Lexing.position
| StrLength of expr * Lexing.position
| SeqLength of expr * Lexing.position
| BVCast of int * expr * Lexing.position
| UbvToInt of expr * Lexing.position
| SbvToInt of expr * Lexing.position
(* First string list track the context of the nonterminal being matched 
   Int options are for clarifying ambiguous dot notation references, as in NTExpr *)
| Match of (string * int option) list * (string * int option) * case list * Lexing.position
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
| NTExpr of (string * int option) list * (string * int option) list * Lexing.position
| BVConst of int * bool list * Lexing.position
| BLConst of bool list * Lexing.position
| BConst of bool * Lexing.position
| IntConst of int * Lexing.position
| PhConst of string * Lexing.position
| StrConst of string * Lexing.position
| ReUnion of expr list * Lexing.position
| ReRange of expr * expr * Lexing.position
| StrInRe of expr * expr * Lexing.position
| StrToRe of expr * Lexing.position
| ReStar of expr * Lexing.position
| ReConcat of expr list * Lexing.position

type semantic_constraint = 
| DerivedField of string * expr * Lexing.position (* <nonterminal> <- <expression> *)
| SmtConstraint of expr * Lexing.position (* Any boolean expression *)

type grammar_element = 
| Nonterminal of string * int option * Lexing.position
| StubbedNonterminal of string * string (* Ignore *)

type prod_rule_rhs = 
| Rhs of grammar_element list * semantic_constraint list * Lexing.position
| StubbedRhs of string (* Ignore *)

type element = 
| ProdRule of string * prod_rule_rhs list * Lexing.position
| TypeAnnotation of string * il_type * semantic_constraint list * Lexing.position

(* This is the type of the grammar terms *)
type ast = element list

(* This function is used before desugaring dot expressions *)
(* TODO: Why is this function returning a string list rather than a string list list? *)
let rec get_nts_from_expr: expr -> string list 
= fun expr -> 
  let r = get_nts_from_expr in
  match expr with 
  | NTExpr (_, nts, _) -> List.map fst nts 
  | Match (_, (nt, _), cases, _) -> nt :: (List.map (fun case -> match case with 
    | CaseStub _ -> []
    | Case (_, expr) -> r expr
    ) cases |> List.flatten)
  | BinOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | UnOp (_, expr, _) -> 
    r expr
  | StrInRe (expr1, expr2, _) 
  | ReRange (expr1, expr2, _)
  | CompOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | StrLength (expr, _)
  | SeqLength (expr, _)
  | StrToRe (expr, _) 
  | ReStar (expr, _) 
  | Length (expr, _) -> 
    r expr
  | Singleton (expr, _) -> r expr
  | ReConcat (exprs, _) 
  | ReUnion (exprs, _) -> 
    List.concat_map r exprs
  | BVCast (_, expr, _) 
  | UbvToInt (expr, _) 
  | SbvToInt (expr, _) -> r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _
  | EmptySet _  -> [] 

(* This function is used before desugaring dot expressions *)
let rec get_nts_from_expr2: expr -> (string * int option) list list
= fun expr -> 
  let r = get_nts_from_expr2 in
  match expr with 
  | NTExpr (nts1, nts2, _) -> [nts1 @ nts2]
  | Match _ -> Utils.crash "Unexpected case in get_nts_from_expr2"
  | BinOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | UnOp (_, expr, _) -> 
    r expr
  | ReRange (expr1, expr2, _) 
  | StrInRe (expr1, expr2, _)
  | CompOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | StrLength (expr, _)
  | SeqLength (expr, _)
  | StrToRe (expr, _)
  | Length (expr, _) -> 
    r expr
  | ReStar (expr, _) 
  | Singleton (expr, _) -> r expr
  | ReConcat (exprs, _) 
  | ReUnion (exprs, _) -> 
    List.concat_map r exprs
  | UbvToInt (expr, _) 
  | BVCast (_, expr, _)  
  | SbvToInt (expr, _) -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _ 
  | EmptySet _ -> []

(* For when you want to process simple NTs after translation of dot to match expressions *)
let rec get_nts_from_expr_after_desugaring_dot_notation: expr -> (string * int option) list list 
= fun expr -> 
  let r = get_nts_from_expr_after_desugaring_dot_notation in
  match expr with 
  | NTExpr (nt_ctx, (nt, idx) :: _, _) -> [nt_ctx @ [nt, idx]] 
  | NTExpr _ -> Utils.crash "Impossible case in get_nts_from_expr_after_desugaring_dot_notation"
  | Match (nt_ctx, (nt, idx), cases, _) -> (nt_ctx @ [nt, idx]) :: 
    (List.map (fun case -> match case with 
    | CaseStub pattern -> List.map (fun (nt_ctx, (nt, idx)) -> nt_ctx @ [nt, idx]) pattern
    | Case (pattern, expr) -> 
      List.map (fun (nt_ctx, (nt, idx)) -> nt_ctx @ [nt, idx]) pattern @ r expr
    ) cases |> List.flatten)
  | ReRange (expr1, expr2, _) 
  | StrInRe (expr1, expr2, _)
  | BinOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | ReStar (expr, _) 
  | UnOp (_, expr, _) -> 
    r expr
  | CompOp (expr1, _, expr2, _) -> 
    r expr1 @ r expr2
  | StrLength (expr, _)
  | SeqLength (expr, _)
  | StrToRe (expr, _) 
  | Length (expr, _) -> 
    r expr
  | Singleton (expr, _) -> r expr
  | ReConcat (exprs, _) 
  | ReUnion (exprs, _) -> 
    List.concat_map r exprs 
  | BVCast (_, expr, _)  
  | UbvToInt (expr, _) 
  | SbvToInt (expr, _) -> 
    r expr
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _ 
  | EmptySet _ -> []

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
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies -> Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "div"
| Mod -> Format.fprintf ppf "mod"
| StrConcat -> Format.fprintf ppf ("str.++")
| SetMembership | SetUnion | SetIntersection -> 
  Utils.crash "Unexpected case in pp_print_bin_op"

let pp_print_unary_op: Format.formatter -> unary_operator -> unit 
= fun ppf op -> match op with 
| UPlus -> Format.fprintf ppf  "+"
| UMinus -> Format.fprintf ppf  "-"
| LNot -> Format.fprintf ppf  "not"
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
| StrContains -> Format.fprintf ppf "str.contains"

(* let pp_print_bit: Format.formatter -> bool ->  *)

let pp_print_pattern: Format.formatter -> ((string * int option) list * (string * int option)) -> unit
= fun ppf (nt_ctx, (nt, idx)) -> 
    pp_print_nt_with_underscores ppf (nt_ctx @ [nt, idx])

let rec pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Unit -> Format.printf "Unit"
| Bool -> Format.fprintf ppf "Bool"
| Int -> Format.fprintf ppf "Int"
| Placeholder -> Format.fprintf ppf "Placeholder"
| String -> Format.fprintf ppf "String"
| BitList -> Format.fprintf ppf "BitList" 
| BitVector width -> Format.fprintf ppf "BitVec(%d)" width
| Set ty -> Format.fprintf ppf "Set(%a)" pp_print_ty ty
| ADT rules -> 
  Format.fprintf ppf "ADT: %a"
    (Lib.pp_print_list (Lib.pp_print_list Format.pp_print_string " ") "; ") rules
    
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
= fun ppf expr -> let r = pp_print_expr in match expr with
| EmptySet (ty, _) -> 
  Format.fprintf ppf "set.empty<%a>"
    pp_print_ty ty
| Singleton (expr, _) -> 
  Format.fprintf ppf "set.singleton(%a)" 
    pp_print_expr expr
| BinOp (expr1, SetMembership, expr2, _) -> 
  Format.fprintf ppf "set.member(%a, %a)"
    pp_print_expr expr1
    pp_print_expr expr2
| BinOp (expr1, SetUnion, expr2, _) -> 
  Format.fprintf ppf "set.union(%a, %a)"
    pp_print_expr expr1
    pp_print_expr expr2
| BinOp (expr1, SetIntersection, expr2, _) -> 
  Format.fprintf ppf "set.intersect(%a, %a)"
    pp_print_expr expr1
    pp_print_expr expr2
| BinOp (expr1, op, expr2, _) -> 
  Format.fprintf ppf "(%a %a %a)" 
    pp_print_expr expr1 
    pp_print_bin_op op 
    pp_print_expr expr2
| UnOp (op, expr, _) -> 
  Format.fprintf ppf "(%a %a)" 
    pp_print_unary_op op 
    pp_print_expr expr
| CompOp (expr1, op, expr2, _) -> 
  Format.fprintf ppf "(%a %a %a)" 
    pp_print_expr expr1 
    pp_print_comp_op op 
    pp_print_expr expr2
| StrLength (expr, _) -> 
  Format.fprintf ppf "str.len(%a)"
    pp_print_expr expr 
| SeqLength (expr, _) -> 
  Format.fprintf ppf "seq.len(%a)"
    pp_print_expr expr 
| Length (expr, _) -> 
  Format.fprintf ppf "length(%a)"
    pp_print_expr expr 
| BVCast (width, expr, _) -> 
  Format.fprintf ppf "int_to_bv(%d, %a)" 
    width 
    pp_print_expr expr 
| UbvToInt (expr, _) -> 
  Format.fprintf ppf "ubv_to_int(%a)" 
    pp_print_expr expr 
| SbvToInt (expr, _) -> 
  Format.fprintf ppf "sbv_to_int(%a)" 
    pp_print_expr expr 
| Match (nts, nt, cases, _) -> 
  Format.fprintf ppf "(match %a with %a)"
    pp_print_nt_with_underscores (nts @ [nt])
    (Lib.pp_print_list pp_print_case " ") cases 
| NTExpr (nts, [nt_expr], _) -> 
  if !Flags.dump_clp then 
    let s = nt_expr |> fst |> String.uppercase_ascii in
    Format.pp_print_string ppf s
  else pp_print_nt_with_underscores ppf (nts @ [nt_expr]) 
| NTExpr ([], nt_expr, _) -> 
  if !Flags.dump_clp then 
    let s = Utils.last nt_expr |> fst |> (fun x -> x ^ "s") |> String.uppercase_ascii in
    let init = Utils.init nt_expr |> List.map fst |> String.concat "_" in
    Format.pp_print_string ppf (init ^ "_" ^ s)
  else pp_print_nt_with_dots ppf (nt_expr) 
| NTExpr (nt_ctx, nts, _) -> 
  Format.fprintf ppf "%a:%a"
    pp_print_nt_with_underscores nt_ctx 
    pp_print_nt_with_dots nts
| BLConst (bits, _) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "(BitList 0b%a)"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BVConst (_, bits, _) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BConst (b, _) -> Format.fprintf ppf "%b" b
| IntConst (i, _) -> Format.fprintf ppf "%d" i
| PhConst (s, _) -> Format.fprintf ppf "\"%s\"" s
| StrConst (s, _) -> Format.fprintf ppf "\"%s\"" s
| StrToRe (e, _) -> 
  Format.fprintf ppf "(str.to_re %a)" 
    r e 
| ReStar (e, _) -> 
  Format.fprintf ppf "(re.* %a)" 
    r e 
| StrInRe (e1, e2, _) -> 
  Format.fprintf ppf "(str.in_re %a %a)" 
    r e1 
    r e2
| ReConcat (es, _) -> 
  Format.fprintf ppf "(re.++ %a)" 
    (Lib.pp_print_list r " ") es  
| ReUnion (es, _) -> 
  Format.fprintf ppf "(re.union %a)" 
    (Lib.pp_print_list r " ") es  
| ReRange (e1, e2, _) ->  
  Format.fprintf ppf "(re.range %a %a)" 
    r e1 
    r e2

let pp_print_semantic_constraint: Format.formatter -> semantic_constraint -> unit 
= fun ppf sc -> match sc with 
| DerivedField (nt, expr, _) -> 
  Format.fprintf ppf "<%a> <- %a;"
    Format.pp_print_string nt 
    pp_print_expr expr
| SmtConstraint (expr, _) -> 
  Format.fprintf ppf "%a;"
    pp_print_expr expr

let pp_print_grammar_element: Format.formatter -> grammar_element ->  unit 
= fun ppf g_el -> match g_el with 
| Nonterminal (nt, idx, _) -> pp_print_nt_with_dots ppf [nt, idx]
| StubbedNonterminal (_, stub_id) -> Format.pp_print_string ppf stub_id

let pp_print_prod_rule_rhs: Format.formatter -> prod_rule_rhs -> unit 
= fun ppf rhss -> 
  match rhss with 
| Rhs (ges, [], _) -> 
  Format.fprintf ppf "%a"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
| Rhs (ges, scs, _) ->
  Format.fprintf ppf "%a \n{ %a }"
  (Lib.pp_print_list pp_print_grammar_element " ") ges
  (Lib.pp_print_list pp_print_semantic_constraint " ") scs
| StubbedRhs stub_id -> 
  Format.pp_print_string ppf stub_id

let pp_print_element: Format.formatter -> element ->  unit 
= fun ppf el -> match el with 
| ProdRule (nt, rhss, _) -> 
  Format.fprintf ppf "%a ::= %a;"
    pp_print_nt_with_dots [nt, None]
    (Lib.pp_print_list pp_print_prod_rule_rhs " | ") rhss

| TypeAnnotation (nt, ty, [], _) -> 
  Format.fprintf ppf "%a :: %a;"
    pp_print_nt_with_dots [nt, None] 
    pp_print_ty ty

| TypeAnnotation (nt, ty, scs, _) -> 
  Format.fprintf ppf "%a :: %a { %a };"
    pp_print_nt_with_dots [nt, None] 
    pp_print_ty ty
    (Lib.pp_print_list pp_print_semantic_constraint " ") scs

let pp_print_ast: Format.formatter -> ast ->  unit 
= fun ppf ast -> 
  Format.fprintf ppf "%a\n"
    (Lib.pp_print_list pp_print_element "\n") ast

let il_int_to_bv: int -> int -> Lexing.position -> expr 
= fun length n pos ->
  if n >= (1 lsl length) then
    (* NOTE: If we overflow, return max value *)
    BVConst (length, Utils.replicate true length, pos)
  else
    let rec to_bits acc len n =
      if len = 0 then acc
      else
        let bit = (n land 1) = 1 in
        to_bits (bit :: acc) (len - 1) (n lsr 1)
    in
    let bits = to_bits [] length n in 
    BVConst (length, bits, pos)

let grammar_element_to_string: grammar_element -> string 
= fun grammar_element -> match grammar_element with 
  | Nonterminal (nt2, _, _) -> nt2
  | StubbedNonterminal (_, stub_id) -> stub_id

(* Used before divide and conquer *)
let nts_of_rhs: prod_rule_rhs -> string list 
= fun rhs -> match rhs with 
| Rhs (ges, _, _) -> 
  List.map (fun ge -> match ge with 
  | Nonterminal (nt, _, _) -> nt 
  | StubbedNonterminal (nt, _) -> nt (* TODO: Not sure which tuple element we want, first or second *)
  ) ges
| StubbedRhs _ -> [] 

let rec expr_contains_dangling_nt: Utils.SILSet.t -> expr -> bool 
= fun ctx expr -> 
  let r = expr_contains_dangling_nt ctx in
  match expr with 
  | NTExpr (nt_ctx, nts, _) -> 
    let nt_expr = nt_ctx @ nts in  
    let res = (List.length nt_expr > 1) && 
    not (Utils.SILSet.mem nt_expr ctx) in 
    (* Format.fprintf Format.std_formatter "Set {%a}: Is %a a member?: %b\n"
      (Lib.pp_print_list pp_print_nt_with_underscores ", ") (Utils.SILSet.elements ctx)
      pp_print_nt_with_underscores nt_expr (Utils.SILSet.mem nt_expr ctx); *)
    res
    
  | BinOp (expr1, _, expr2, _) -> 
    r expr1 || r expr2
  | UnOp (_, expr, _) -> 
    r expr
  | CompOp (expr1, _, expr2, _) -> 
    r expr1 || r expr2
  | StrLength (expr, _)
  | SeqLength (expr, _)
  | ReStar (expr, _)  
  | Length (expr, _) -> 
    r expr
  | Singleton (expr, _) -> r expr
  | ReRange (expr1, expr2, _) -> r expr1 || r expr2
  | ReConcat (exprs, _) 
  | ReUnion (exprs, _) -> List.map r exprs |> List.fold_left (||) false  
  | StrInRe (expr1, expr2, _) -> r expr1 || r expr2
  | StrToRe (expr, _) -> r expr
  | BVCast (_, expr, _)  
  | UbvToInt (expr, _) 
  | SbvToInt (expr, _) -> r expr 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _ 
  | EmptySet _ -> false
  | Match _ -> 
    Utils.crash "Encountered Match in expr_contains_dangling_nt. 
    Shouldn't be possible, as this function should only process base expressions."

let sc_constrains_nt: string -> semantic_constraint -> bool 
= fun nt sc -> match sc with 
| SmtConstraint (expr, _) -> List.mem nt (get_nts_from_expr expr)
| DerivedField (nt2, _, _) -> nt = nt2

let get_nts_from_sc: semantic_constraint -> string list 
= fun sc -> match sc with 
| SmtConstraint (expr, _) -> get_nts_from_expr expr
| DerivedField (nt2, _, _) -> [nt2]

(* To be called before desugaring NTs to match expressions and resolving ambiguities.
   This may seem overconservative, but in the sygus_dac approach, we have to have subproblems 
   be completely independent to stub them out. E.g., consider 
   <A> -> <B> { <B>.<C> < 5 };
   <B> -> <C> <D> { <D> < 6 }; 
   It may seem these should not overlap. But in the sygus approach, they must, 
   because otherwise we will try to stub out <B>, but then <B>.<C> will fail.
   *)
let ast_constrains_nt: ast -> string -> bool 
= fun ast nt -> 
  List.exists (fun element -> match element with 
    | TypeAnnotation (nt2, _, _ :: _, _) when nt = nt2 -> true 
    | TypeAnnotation _ -> false
    | ProdRule (_, rhss, _) -> List.exists (fun rhs -> match rhs with 
      | Rhs (_, scs, _) -> List.exists (sc_constrains_nt nt) scs
      | StubbedRhs _ -> false
    ) rhss
  ) ast

(* Use before resolving constraint ambiguities *)
let rec prepend_nt_to_dot_exprs: string -> expr -> expr 
= fun nt expr -> 
  let r = prepend_nt_to_dot_exprs nt in
  match expr with
  | NTExpr ([], nts, pos) -> NTExpr ([], (nt, None) :: nts, pos)
  | BVCast (len, expr, pos) -> BVCast (len, r expr, pos)
  | UbvToInt (expr, pos) -> UbvToInt (r expr, pos)
  | SbvToInt (expr, pos) -> SbvToInt (r expr, pos)
  | BinOp (expr1, op, expr2, pos) -> BinOp (r expr1, op, r expr2, pos) 
  | UnOp (op, expr, pos) -> UnOp (op, r expr, pos) 
  | CompOp (expr1, op, expr2, pos) -> CompOp (r expr1, op, r expr2, pos) 
  | StrLength (expr, pos) -> StrLength (r expr, pos)
  | Length (expr, pos) -> Length (r expr, pos) 
  | SeqLength (expr, pos) -> SeqLength (r expr, pos) 
  | Singleton (expr, pos) -> Singleton (r expr, pos)
  | ReRange (expr1, expr2, pos) -> ReRange (r expr1, r expr2, pos) 
  | ReConcat (exprs, pos) -> ReConcat (List.map r exprs, pos)
  | ReUnion (exprs, pos) -> ReUnion (List.map r exprs, pos) 
  | StrInRe (expr1, expr2, pos) -> StrInRe (r expr1, r expr2, pos)
  | ReStar (expr, pos) -> ReStar (r expr, pos)
  | StrToRe (expr, pos) -> StrToRe (r expr, pos)
  | Match _ -> Utils.crash "Unexpected case 1 in prepend_nt_to_dot_exprs"
  | NTExpr _ -> Utils.crash "Unexpected case 2 in prepend_nt_to_dot_exprs" 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _
  | EmptySet _ -> expr

let scs_of_element = function 
| ProdRule (_, rhss, _) -> 
  List.concat_map (function 
  | StubbedRhs _ -> [] 
  | Rhs (_, scs, _) -> scs
  ) rhss
| TypeAnnotation (_, _, scs, _) -> scs

let rec nts_of_ast ast = match ast with 
| [] -> Utils.StringSet.empty  
| ProdRule (nt, rhss, _) :: tl -> 
  let nts = nt :: (List.concat_map nts_of_rhs rhss) in 
  let nts = Utils.StringSet.of_list nts in 
  Utils.StringSet.union nts (nts_of_ast tl)
| TypeAnnotation (nt, _, _, _) :: tl -> 
  let nts = Utils.StringSet.singleton nt in 
  Utils.StringSet.union nts (nts_of_ast tl)

