module A = Ast
module TC = TypeChecker
module StringMap = Utils.StringMap

let fresh_destructor: unit -> string 
= fun () ->
  let id = "des" ^ string_of_int !Utils.k in 
  Utils.k := !Utils.k + 1;
  id

let fresh_constraint: unit -> string 
= fun () ->
  let id = "c" ^ string_of_int !Utils.k in 
  Utils.k := !Utils.k + 1;
  id

let pp_print_ty: Format.formatter -> A.il_type -> unit 
= fun ppf ty -> match ty with 
| Int -> Format.fprintf ppf "Int"
| Bool -> Format.fprintf ppf "Bool"
(* NOTE: Strings should only be present for dependency computations. 
   If it's here, it is a grammar element that would be pruned anyway. *)
| Placeholder -> Format.fprintf ppf "Int"
| String -> Format.fprintf ppf "String"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| ADT _ -> Utils.crash "sygus.ml (pp_print_ty)"

let pp_print_constructor: TC.context -> Ast.semantic_constraint Utils.StringMap.t -> Ast.ast ->  Format.formatter -> A.grammar_element -> unit 
= fun ctx dep_map ast ppf ge -> match ge with 
| A.Nonterminal nt ->
  let d_str = fresh_destructor () in 
  let ty_str = 
  match Utils.StringMap.find_opt nt dep_map,
        Utils.StringMap.find_opt nt ctx with 
  | None, None 
  | Some _, _ -> String.uppercase_ascii nt
  | _, Some ty -> 
    let type_annot = List.find_opt (fun element -> match element with 
    | A.ProdRule _ -> false 
    | TypeAnnotation (nt2, _, _) -> nt = nt2 
    ) ast
    in (
    match type_annot with 
    | None -> String.uppercase_ascii nt
    | Some _ -> 
      Utils.capture_output pp_print_ty ty
    )
  in
  Format.fprintf ppf "(%s %s)"
    d_str
    ty_str
| StubbedNonterminal (_, stub_id) -> 
  Format.fprintf ppf "(%s %s)"
    (fresh_destructor ())
    (String.uppercase_ascii stub_id)

let pp_print_datatype_rhs 
= fun ctx dep_map nt ast ppf (rhs, idx) -> match rhs with 
| A.Rhs (ges, _) -> 
  Format.fprintf ppf "\n\t(%s %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx))
    (Lib.pp_print_list (pp_print_constructor ctx dep_map ast) " ") ges
| StubbedRhs stub_id -> 
  Format.fprintf ppf "\n\t(%s)"
  ((String.lowercase_ascii stub_id) ^ "_con" ^ (string_of_int idx))

let pp_print_datatypes: Format.formatter -> TC.context -> Ast.semantic_constraint Utils.StringMap.t -> A.ast -> unit 
= fun ppf ctx dep_map ast -> 
  Utils.StringMap.iter (fun stub_id dep -> match dep with 
  | A.Dependency _ -> 
    Format.fprintf ppf "(declare-datatype %s (\n\t(%s)\n))\n"
      (String.uppercase_ascii stub_id)
      ((String.lowercase_ascii stub_id) ^ "_con")
  | SyGuSExpr _ -> Utils.crash "dependency map contains a SyGuSExpr"
  ) dep_map;
  List.iter (fun element -> match element with 
  | A.TypeAnnotation _ -> ()
  | ProdRule (nt, rhss) -> 
    Format.fprintf ppf "(declare-datatype %s (%a\n))\n"
      (String.uppercase_ascii nt)
      (Lib.pp_print_list (pp_print_datatype_rhs ctx dep_map nt ast) " ") (List.mapi (fun i rhs -> (rhs, i)) rhss);
  ) ast 

let pp_print_unop: Format.formatter -> A.unary_operator -> unit 
= fun ppf op -> match op with 
| A.UPlus -> ()
| UMinus -> Format.fprintf ppf "-"
| LNot -> Format.fprintf ppf "not"
| BVNot -> Format.fprintf ppf "bvnot"

let pp_print_binop: Format.formatter -> A.bin_operator -> unit 
= fun ppf op -> match op with 
| A.BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> Utils.crash "Should not reach this case in pp_print_binop"
| GLAnd
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies ->Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"
| StrConcat -> Format.fprintf ppf "str.++"

let pp_print_compop: Format.formatter -> A.comp_operator -> unit 
= fun ppf op -> match op with 
| A.Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="
| _ -> assert false


let pp_print_nt_decs: Ast.semantic_constraint Utils.StringMap.t -> Format.formatter -> A.ast -> unit 
= fun dep_map ppf ast -> List.iter (fun element -> match element with 
| A.ProdRule (nt, _) ->
  Format.fprintf ppf "\t(%s %s)\n"
  (String.lowercase_ascii nt)
  (String.uppercase_ascii nt) 
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a)\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
) ast;
Utils.StringMap.iter (fun stub_id dep -> match dep with 
| A.Dependency _ -> 
  Format.fprintf ppf "\t(%s %s)"
  (String.lowercase_ascii stub_id) 
  (String.uppercase_ascii stub_id) 
| SyGuSExpr _ -> Utils.crash "dependency map contains a SyGuSExpr"
) dep_map

let rec pp_print_match: Format.formatter -> TC.context -> (string * int option) list -> string * int option -> A.case list -> unit 
= fun ppf ctx nt_ctx (nt, idx) cases -> 
  let adt_cases = match StringMap.find nt ctx with 
  | ADT rules -> rules
  | _ -> Utils.crash "sygus.ml (pp_print_match)" 
  in
  let match_rules = List.map (fun case -> match case with 
  | A.Case (nts, expr) -> nts, expr 
  | CaseStub nts -> nts, BConst true 
  ) cases in 
  let match_rules = List.map (fun (pattern, expr) -> 
    let original_nts = List.map (fun (_, (b, _)) -> b) pattern in
    let rule_index = Lib.find_index original_nts adt_cases in 
    ((rule_index, pattern), expr)
  ) match_rules in

  Format.fprintf ppf "(match %a (
    %a
  ))"
    (Lib.pp_print_list pp_print_nt_helper "_") (nt_ctx @ [nt, idx])
    (Lib.pp_print_list (fun ppf -> Format.fprintf ppf "(%a)" (pp_print_option ctx nt)) " ") match_rules

and pp_print_option: TC.context -> string -> Format.formatter -> ((int * (((string * int option) list * (string * int option)) list)) * A.expr) -> unit 
= fun ctx nt ppf ((i, pattern), expr) -> 
  let pattern = List.map (fun (nt_ctx, (nt, idx)) -> nt_ctx @ [nt, idx]) pattern in
    Format.fprintf ppf "(%s_con%d %a) %a"
    (String.lowercase_ascii nt)
    i
    (Lib.pp_print_list (fun t -> (Lib.pp_print_list pp_print_nt_helper "_") t) " ") pattern
    (pp_print_expr ctx) expr 

and pp_print_nt_helper 
= fun ppf (str, idx) -> 
  Format.fprintf ppf "%s%s"
    (String.lowercase_ascii str) 
    (match idx with 
    | None -> ""
    | Some i -> string_of_int i)

(* The NT prefix is only used in the dpll.ml module *)
and pp_print_expr: ?nt_prefix:string -> TC.context -> Format.formatter -> A.expr -> unit 
= fun ?(nt_prefix="") ctx ppf expr -> 
  let r = pp_print_expr ~nt_prefix ctx in
  match expr with 
  | NTExpr (nt_ctx, [nt]) ->
    (* TODO: Use a representation that prevents name clashes with user names *)
    let nts = List.map (fun (str, idx) -> String.lowercase_ascii str, idx) (nt_ctx @ [nt]) in
    (if not (String.equal nt_prefix "") then
      Format.pp_print_string ppf (nt_prefix ^ "_"));
    Lib.pp_print_list pp_print_nt_helper "_" ppf nts
  | A.Match (nt_ctx, nt, cases)  -> pp_print_match ppf ctx nt_ctx nt cases
  | NTExpr (nts1, nts2) -> 
    let nt_ctx = nts1 @ Utils.init nts2 in 
    let nt = nts2 |> List.rev |> List.hd in 
    r ppf (Ast.NTExpr (nt_ctx, [nt]))
  | BinOp (expr1, BVXor, expr2) -> 
    Format.fprintf ppf "(and (or %a %a) (not (and %a %a)))"
      r expr1 
      r expr2
      r expr1 
      r expr2
  | BinOp (expr1, op, expr2) -> 
    Format.fprintf ppf "(%a %a %a)"
      pp_print_binop op 
      r expr1 
      r expr2
  | CompOp (expr1, BVLt, expr2) -> 
    Format.fprintf ppf "(bvult %a %a)"
      r expr1 
      r expr2
  | CompOp (expr1, BVLte, expr2) -> 
    Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
      r expr1 
      r expr2
      r expr1 
      r expr2
  | CompOp (expr1, BVGt, expr2) -> 
    Format.fprintf ppf "(bvult %a %a)"
      r expr2
      r expr1 
  | CompOp (expr1, BVGte, expr2) -> 
    Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
      r expr2
      r expr1 
      r expr2
      r expr1 
  | CompOp (expr1, StrPrefix, expr2) -> 
    Format.fprintf ppf "(str.prefixof %a %a)"
      r expr1
      r expr2 
  | CompOp (expr1, op, expr2) -> 
    Format.fprintf ppf "(%a %a %a)"
      pp_print_compop op 
      r expr1 
      r expr2
  | UnOp (op, expr) -> 
    Format.fprintf ppf "(%a %a)"
      pp_print_unop op 
      r expr
  | StrLength expr -> 
    Format.fprintf ppf "(str.len %a)"
      r expr
  | Length expr -> 
    Format.fprintf ppf "(seq.len %a)"
      r expr
  | BVConst (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "#b%a"
      (Lib.pp_print_list Format.pp_print_int "") bits
  | BConst b ->  Format.fprintf ppf "%b" b
  | IntConst i -> Format.fprintf ppf "%d" i
  | StrConst str -> Format.pp_print_string ppf str
  | PhConst _ -> Utils.crash "Error: String constants can only be in dependencies (of the form 'nonterminal <- string_literal')"
  | BLConst _ -> Utils.crash "BitList literals not yet fully supported"
  | BVCast _ -> Utils.crash "Integer to bitvector casts in semantic constraints that aren't preprocessable are not supported"

let pp_print_semantic_constraint_ty_annot: TC.context -> Format.formatter -> string -> A.il_type -> A.semantic_constraint -> unit 
= fun ctx ppf nt ty sc -> match sc with 
| A.Dependency _ -> () 
| SyGuSExpr expr -> 
  let constraint_id = fresh_constraint () in 
  Format.fprintf ppf "(define-fun %s ((%s %a)) Bool \n\t%a\n)\n"
    constraint_id
    (String.lowercase_ascii nt) 
    pp_print_ty ty
    (pp_print_expr ctx) expr;
  Format.fprintf ppf "(constraint (%s top))"
    constraint_id


let find_indices lst =
  let occurrences = Hashtbl.create (List.length lst) in
  
  (* Compute occurrences of each string *)
  List.iter (fun s ->
    Hashtbl.replace occurrences s (match Hashtbl.find_opt occurrences s with
      | None -> 1
      | Some count -> count + 1
    )
  ) lst;

  (* Generate the result with indices for repeated elements *)
  let counts = Hashtbl.create (List.length lst) in
  List.map (fun s ->
    let count = Hashtbl.find occurrences s in
    if count = 1 then (s, None)
    else 
      let index = (Hashtbl.find_opt counts s |> Option.value ~default:0) + 1 in
      Hashtbl.replace counts s index;
      (s, Some (index - 1))
  ) lst

let pp_print_ges_pattern: Format.formatter -> string list -> unit 
= fun ppf ges -> 
  let ges = find_indices ges in 
  Format.fprintf ppf "%a"
    (Lib.pp_print_list pp_print_nt_helper " ") ges

let pp_print_constraints_rhs: TC.context -> string -> Format.formatter -> A.prod_rule_rhs * int -> unit
= fun ctx nt ppf (rhs, idx) -> match rhs with 
| StubbedRhs _ -> Format.fprintf ppf "2STUB\n"
| Rhs (ges, scs) ->
  let 
    ges = List.map Ast.grammar_element_to_string ges |> List.map String.lowercase_ascii 
  in  
  let exprs = List.filter_map (fun sc -> match sc with 
  | A.SyGuSExpr expr -> Some expr 
  | _ -> None
  ) scs in
  if List.length exprs > 1 then
    Format.fprintf ppf "((%s %a)\n\t\t (and %a))"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      pp_print_ges_pattern ges
      (Lib.pp_print_list (pp_print_expr ctx) " ") exprs
  else if List.length exprs = 1 then 
    Format.fprintf ppf "((%s %a)\n\t\t %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      pp_print_ges_pattern ges
      (Lib.pp_print_list (pp_print_expr ctx) " ") exprs 
  else 
    Format.fprintf ppf "((%s %a)\n\t\t true)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      pp_print_ges_pattern ges
  
let pp_print_semantic_constraints_prod_rule
= fun ctx ppf nt rhss ->
  let constraint_id = fresh_constraint () in 

  Format.fprintf ppf "(define-fun %s ((%s %s)) Bool \n\t(match %s (\n\t\t%a\n\t))\n)\n"
  constraint_id
    (String.lowercase_ascii nt) 
    (String.uppercase_ascii nt) 
    (String.lowercase_ascii nt)
    (Lib.pp_print_list (pp_print_constraints_rhs ctx nt) "\n") rhss;
  Format.fprintf ppf "(constraint (%s top))"
    constraint_id

let pp_print_constraints: TC.context -> Format.formatter -> A.ast -> unit 
= fun ctx ppf ast -> match ast with 
| [] -> Utils.crash "Input grammar must have at least one production rule or type annotation"
| A.ProdRule (nt, rhss) :: _ -> 
  if List.exists (fun rhs -> match rhs with | A.Rhs (_, _ :: _) -> true | _ -> false) rhss then
  let rhss = List.mapi (fun i rhs -> (rhs, i)) rhss in
  pp_print_semantic_constraints_prod_rule ctx ppf nt rhss
| TypeAnnotation (nt, ty, scs) :: _ -> 
  List.iter (pp_print_semantic_constraint_ty_annot ctx ppf nt ty) scs

let pp_print_rhs: string -> Format.formatter -> A.prod_rule_rhs * int -> unit
= fun nt ppf (rhs, idx) -> match rhs with 
| A.Rhs (ges, _) ->
  let ges = List.map Ast.grammar_element_to_string ges in 
  let ges = List.map String.lowercase_ascii ges in
  Format.fprintf ppf "(%s %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx))
    (Lib.pp_print_list Format.pp_print_string " ") ges
| StubbedRhs stub_id -> 
  Format.fprintf ppf "%s"
    ((String.lowercase_ascii stub_id) ^ "_con" ^ (string_of_int idx))

let pp_print_rules: Ast.semantic_constraint Utils.StringMap.t -> Format.formatter -> A.ast -> unit 
= fun dep_map ppf ast -> 
  List.iter (fun element -> match element with 
  | A.ProdRule (nt, rhss) -> 
    Format.fprintf ppf "\t(%s %s (%a))\n"
      (String.lowercase_ascii nt) 
      (String.uppercase_ascii nt) 
      (Lib.pp_print_list (pp_print_rhs nt) " ") (List.mapi (fun i rhs -> (rhs, i)) rhss)
  | TypeAnnotation (nt, ty, _) -> 
    Format.fprintf ppf "\t(%s %a ((Constant %a)))\n"
      (String.lowercase_ascii nt) 
      pp_print_ty ty
      pp_print_ty ty
  ) ast;
  Utils.StringMap.iter (fun stub_id dep -> match dep with 
  | A.Dependency _ -> 
    Format.fprintf ppf "\t(%s %s (%s))"
      (String.lowercase_ascii stub_id) 
      (String.uppercase_ascii stub_id) 
      ((String.lowercase_ascii stub_id) ^ "_con")
  | SyGuSExpr _ -> Utils.crash "dependency map contains a SyGuSExpr"
  ) dep_map

let pp_print_grammar: Format.formatter -> Ast.semantic_constraint Utils.StringMap.t -> A.ast -> unit 
= fun ppf dep_map ast -> 
  let top_datatype_str = match List.hd ast with 
  | A.ProdRule (nt, _) -> String.uppercase_ascii nt
  | TypeAnnotation (_, ty, _) -> 
    Utils.capture_output pp_print_ty ty
  in
  Format.fprintf ppf 
    "(synth-fun top () %s\n; declare nonterminals\n(\n%a\n)\n; grammar rules\n(\n%a\n)\n)"
    top_datatype_str
    (pp_print_nt_decs dep_map) ast 
    (pp_print_rules dep_map) ast

let pp_print_ast: Format.formatter -> (TC.context * Ast.semantic_constraint Utils.StringMap.t * A.ast) -> unit 
= fun ppf (ctx, dep_map, ast) -> 
  Format.fprintf ppf "(set-logic BVSLIA)\n\n";

  pp_print_datatypes ppf ctx dep_map (List.rev ast);

  Lib.pp_print_newline ppf ();

  pp_print_grammar ppf dep_map ast;
  
  Lib.pp_print_newline ppf ();
  Lib.pp_print_newline ppf ();

  pp_print_constraints ctx ppf ast;

  Lib.pp_print_newline ppf ();
  Lib.pp_print_newline ppf ();

  Format.fprintf ppf "(check-synth)";

  Lib.pp_print_newline ppf ()

let call_sygus : TC.context -> Ast.semantic_constraint Utils.StringMap.t -> A.ast -> string =
fun ctx dep_map ast ->
  let top_nt = match ast with
  | ProdRule (nt, _) :: _ -> nt
  | TypeAnnotation (nt, _, _) :: _ -> nt
  | _ -> assert false
  in
  
  let input_filename = Filename.temp_file (top_nt ^ "_input_") ".smt2" in
  let output_filename = Filename.temp_file (top_nt ^ "_out1_") ".smt2" in
  let output_filename2 = Filename.temp_file (top_nt ^ "_out2_") ".smt2" in

  (* Create sygus input file *)
  let oc = open_out input_filename in
  let ppf = Format.formatter_of_out_channel oc in
  pp_print_ast ppf (ctx, dep_map, ast);
  Format.pp_print_flush ppf ();
  close_out oc;

  (* Call sygus command *)
  let cvc5 = Utils.find_command_in_path "cvc5" in
  let cvc5_2 = match Sys.getenv_opt "PATH_TO_SECOND_CVC5" with 
  (* let cvc5_2 = find_command_in_path "cvc5" in *)
  | Some path -> path 
  | None -> Utils.warning_print Format.pp_print_string Format.std_formatter "Proceeding without a second cvc5 config\n"; cvc5 
  in
  let command = Printf.sprintf "timeout 3 %s --lang=sygus2 --dag-thresh=0 %s > %s" cvc5 input_filename output_filename in
  let command2 = Printf.sprintf "timeout 3 %s --lang=sygus2 --dag-thresh=0 %s > %s" cvc5_2 input_filename output_filename2 in
  (* Run two versions of sygus in parallel and use results from whichever finishes first *)
  if Parallelism.race_commands command command2 then (
    let ic = open_in output_filename in
    let len = in_channel_length ic in
    let output = really_input_string ic len in
    close_in ic;
    output
  ) else (
    let ic = open_in output_filename2 in
    let len = in_channel_length ic in
    let output = really_input_string ic len in
    close_in ic;
    output
  )