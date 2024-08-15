open Ast

module TC = TypeChecker

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

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Int -> Format.fprintf ppf "Int"
| Bool -> Format.fprintf ppf "Bool"
(* Strings should only be present for dependency computations. 
   If it's here, it is a grammar element that would be pruned anyway. *)
| String -> Format.fprintf ppf "Int"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| MachineInt width -> Format.fprintf ppf "(_ BitVec %d)" (Lib.pow 2 width)

let pp_print_constructor: TC.context -> Ast.semantic_constraint Utils.StringMap.t -> Ast.ast ->  Format.formatter -> grammar_element -> unit 
= fun ctx dep_map ast ppf ge -> match ge with 
| Nonterminal nt 
| NamedNonterminal (_, nt) -> 
  let d_str = fresh_destructor () in 
  let ty_str = 
  match Utils.StringMap.find_opt nt dep_map,
        Utils.StringMap.find_opt nt ctx with 
  | None, None 
  | Some _, _ -> String.uppercase_ascii nt
  | _, Some ty -> 
    let type_annot = List.find_opt (fun element -> match element with 
    | ProdRule _ -> false 
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
| Rhs (ges, _) -> 
  Format.fprintf ppf "\n\t(%s %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx))
    (Lib.pp_print_list (pp_print_constructor ctx dep_map ast) " ") ges
| StubbedRhs stub_id -> 
  Format.fprintf ppf "\n\t(%s)"
  ((String.lowercase_ascii stub_id) ^ "_con" ^ (string_of_int idx))

let pp_print_datatypes: Format.formatter -> TC.context -> Ast.semantic_constraint Utils.StringMap.t -> ast -> unit 
= fun ppf ctx dep_map ast -> 
  Utils.StringMap.iter (fun stub_id dep -> match dep with 
  | Dependency _ -> 
    Format.fprintf ppf "(declare-datatype %s (\n\t(%s)\n))\n"
      (String.uppercase_ascii stub_id)
      ((String.lowercase_ascii stub_id) ^ "_con")
  | SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
  ) dep_map;
  List.iter (fun element -> match element with 
  | TypeAnnotation _ -> ()
  | ProdRule (nt, rhss) -> 
    Format.fprintf ppf "(declare-datatype %s (%a\n))\n"
      (String.uppercase_ascii nt)
      (Lib.pp_print_list (pp_print_datatype_rhs ctx dep_map nt ast) " ") (List.mapi (fun i rhs -> (rhs, i)) rhss);
  ) ast 

let pp_print_nt_decs: Ast.semantic_constraint Utils.StringMap.t -> Format.formatter -> ast -> unit 
= fun dep_map ppf ast -> List.iter (fun element -> match element with 
| ProdRule (nt, _) ->
  Format.fprintf ppf "\t(%s %s)\n"
  (String.lowercase_ascii nt)
  (String.uppercase_ascii nt) 
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a)\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
) ast;
Utils.StringMap.iter (fun stub_id dep -> match dep with 
| Dependency _ -> 
  Format.fprintf ppf "\t(%s %s)"
  (String.lowercase_ascii stub_id) 
  (String.uppercase_ascii stub_id) 
| SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
) dep_map

let pp_print_binop: Format.formatter -> bin_operator -> unit 
= fun ppf op -> match op with 
| BVAnd -> Format.fprintf ppf "bvand"
| BVOr -> Format.fprintf ppf "bvor"
| BVXor -> failwith "BitVector xor is not supported"
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies ->Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"

let pp_print_compop: Format.formatter -> comp_operator -> unit 
= fun ppf op -> match op with 
| Lt -> Format.fprintf ppf "<"
| Lte -> Format.fprintf ppf "<="
| Gt -> Format.fprintf ppf ">"
| Gte -> Format.fprintf ppf ">="
| Eq -> Format.fprintf ppf "="
| _ -> assert false

let pp_print_unop: Format.formatter -> unary_operator -> unit 
= fun ppf op -> match op with 
| UPlus -> ()
| UMinus -> Format.fprintf ppf "-"
| LNot -> Format.fprintf ppf "not"
| BVNot -> Format.fprintf ppf "bvnot"

let rec pp_print_expr: Format.formatter -> expr -> unit 
= fun ppf expr -> match expr with 
| NTExpr ([nt], None) -> Format.pp_print_string ppf (String.lowercase_ascii nt)
| BinOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
    pp_print_binop op 
    pp_print_expr expr1 
    pp_print_expr expr2
| CompOp (expr1, BVLt, expr2) -> 
  Format.fprintf ppf "(bvult %a %a)"
    pp_print_expr expr1 
    pp_print_expr expr2
| CompOp (expr1, BVLte, expr2) -> 
  Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
    pp_print_expr expr1 
    pp_print_expr expr2
    pp_print_expr expr1 
    pp_print_expr expr2
| CompOp (expr1, BVGt, expr2) -> 
  Format.fprintf ppf "(bvult %a %a)"
    pp_print_expr expr2
    pp_print_expr expr1 
| CompOp (expr1, BVGte, expr2) -> 
  Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
    pp_print_expr expr2
    pp_print_expr expr1 
    pp_print_expr expr2
    pp_print_expr expr1 
| CompOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
    pp_print_compop op 
    pp_print_expr expr1 
    pp_print_expr expr2
| UnOp (op, expr) -> 
  Format.fprintf ppf "(%a %a)"
    pp_print_unop op 
    pp_print_expr expr
| Length expr -> 
  Format.fprintf ppf "(seq.len %a)"
    pp_print_expr expr
| BVConst (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "#b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BConst b ->  Format.fprintf ppf "%b" b
| IntConst i -> Format.fprintf ppf "%d" i
| StrConst _ -> failwith "Error: String constants can only be in dependencies (of the form 'nonterminal <- string_literal')"
| CaseExpr _  -> failwith "Case expressions not yet fully supported"
| NTExpr _ -> failwith "Nonterminal expressions with either dot notation or indexing are not yet fully supported" 
| BLConst _ -> failwith "BitList literals not yet fully supported"
| BVCast _ -> failwith "Integer to bitvector casts in semantic constraints that aren't preprocessable are not supported"

let pp_print_semantic_constraint_ty_annot: Format.formatter -> string -> il_type -> semantic_constraint -> unit 
= fun ppf nt ty sc -> match sc with 
| Dependency _ -> () 
| SyGuSExpr expr -> 
  let constraint_id = fresh_constraint () in 
  Format.fprintf ppf "(define-fun %s ((%s %a)) Bool \n\t%a\n)\n"
    constraint_id
    (String.lowercase_ascii nt) 
    pp_print_ty ty
    pp_print_expr expr;
  Format.fprintf ppf "(constraint (%s top))"
    constraint_id

let pp_print_constraints_rhs: string -> Format.formatter -> prod_rule_rhs * int -> unit
= fun nt ppf (rhs, idx) -> match rhs with 
| StubbedRhs _ -> Format.fprintf ppf "2STUB\n"
| Rhs (ges, scs) ->
  let 
    ges = List.map Utils.grammar_element_to_string ges |> List.map String.lowercase_ascii 
  in  
  let exprs = List.filter_map (fun sc -> match sc with 
  | SyGuSExpr expr -> Some expr 
  | _ -> None
  ) scs in
  if List.length exprs > 1 then
    Format.fprintf ppf "((%s %a)\n\t\t (and %a))"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
      (Lib.pp_print_list pp_print_expr " ") exprs
  else if List.length exprs = 1 then 
    Format.fprintf ppf "((%s %a)\n\t\t %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
      (Lib.pp_print_list pp_print_expr " ") exprs 
  else 
    Format.fprintf ppf "((%s %a)\n\t\t true)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
  
let pp_print_semantic_constraints_prod_rule
= fun ppf nt rhss ->
  let constraint_id = fresh_constraint () in 

  Format.fprintf ppf "(define-fun %s ((%s %s)) Bool \n\t(match %s (\n\t\t%a\n\t))\n)\n"
  constraint_id
    (String.lowercase_ascii nt) 
    (String.uppercase_ascii nt) 
    (String.lowercase_ascii nt)
    (Lib.pp_print_list (pp_print_constraints_rhs nt) "\n") rhss;
  Format.fprintf ppf "(constraint (%s top))"
    constraint_id

let pp_print_constraints: Format.formatter -> ast -> unit 
= fun ppf ast -> match ast with 
| [] -> failwith "Input grammar must have at least one production rule or type annotation"
| ProdRule (nt, rhss) :: _ -> 
  if List.exists (fun rhs -> match rhs with | Rhs (_, _ :: _) -> true | _ -> false) rhss then
  let rhss = List.mapi (fun i rhs -> (rhs, i)) rhss in
  pp_print_semantic_constraints_prod_rule ppf nt rhss
| TypeAnnotation (nt, ty, scs) :: _ -> 
  List.iter (pp_print_semantic_constraint_ty_annot ppf nt ty) scs

let pp_print_rhs: string -> Format.formatter -> prod_rule_rhs * int -> unit
= fun nt ppf (rhs, idx) -> match rhs with 
| Rhs (ges, _) ->
  let ges = List.map Utils.grammar_element_to_string ges in 
  let ges = List.map String.lowercase_ascii ges in
  Format.fprintf ppf "(%s %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx))
    (Lib.pp_print_list Format.pp_print_string " ") ges
| StubbedRhs stub_id -> 
  Format.fprintf ppf "%s"
    ((String.lowercase_ascii stub_id) ^ "_con" ^ (string_of_int idx))

let pp_print_rules: Ast.semantic_constraint Utils.StringMap.t -> Format.formatter -> ast -> unit 
= fun dep_map ppf ast -> 
  List.iter (fun element -> match element with 
  | ProdRule (nt, rhss) -> 
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
  | Dependency _ -> 
    Format.fprintf ppf "\t(%s %s (%s))"
      (String.lowercase_ascii stub_id) 
      (String.uppercase_ascii stub_id) 
      ((String.lowercase_ascii stub_id) ^ "_con")
  | SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
  ) dep_map

let pp_print_grammar: Format.formatter -> Ast.semantic_constraint Utils.StringMap.t -> ast -> unit 
= fun ppf dep_map ast -> 
  let top_datatype_str = match List.hd ast with 
  | ProdRule (nt, _) -> String.uppercase_ascii nt
  | TypeAnnotation (_, ty, _) -> 
    Utils.capture_output pp_print_ty ty
  in
  Format.fprintf ppf 
    "(synth-fun top () %s\n; declare nonterminals\n(\n%a\n)\n; grammar rules\n(\n%a\n)\n)"
    top_datatype_str
    (pp_print_nt_decs dep_map) ast 
    (pp_print_rules dep_map) ast

let pp_print_ast: Format.formatter -> (TC.context * Ast.semantic_constraint Utils.StringMap.t * ast) -> unit 
= fun ppf (ctx, dep_map, ast) -> 
  Format.fprintf ppf "(set-logic ALL)\n\n";

  pp_print_datatypes ppf ctx dep_map (List.rev ast);

  Lib.pp_print_newline ppf ();

  pp_print_grammar ppf dep_map ast;
  
  Lib.pp_print_newline ppf ();
  Lib.pp_print_newline ppf ();

  pp_print_constraints ppf ast;

  Lib.pp_print_newline ppf ();
  Lib.pp_print_newline ppf ();

  Format.fprintf ppf "(check-synth)";

  Lib.pp_print_newline ppf ()

type result = 
| Command1
| Command2

(* Run two commands in parallel and report which finishes first *)
let run_commands cmd1 cmd2 =
  let pid1 = Unix.create_process "/bin/bash" [| "bash"; "-c"; cmd1 |] Unix.stdin Unix.stdout Unix.stderr in
  let pid2 = Unix.create_process "/bin/bash" [| "bash"; "-c"; cmd2 |] Unix.stdin Unix.stdout Unix.stderr in

  let rec wait_for_first pid1 pid2 =
    let pid, _ = Unix.wait () in
    if pid = pid1 then (
      Unix.kill pid2 Sys.sigterm;
      Command1
    ) else if pid = pid2 then (
      Unix.kill pid1 Sys.sigterm;
      Command2
    ) else
      wait_for_first pid1 pid2
  in

  wait_for_first pid1 pid2

let call_sygus : TC.context -> Ast.semantic_constraint Utils.StringMap.t -> ast -> string =
fun ctx dep_map ast ->
  let top_nt = match ast with
  | ProdRule (nt, _) :: _ -> nt
  | TypeAnnotation (nt, _, _) :: _ -> nt
  | _ -> assert false
  in
  (* ignore (Unix.system "mkdir sygus_debug > /dev/null"); *)
  (* ignore (Unix.system "mkdir sygus_debug > /dev/null"); *)
  let input_filename = "./sygus_debug/" ^ top_nt ^ ".smt2" in
  let output_filename = "./sygus_debug/" ^ top_nt ^ "_out.smt2" in
  let output_filename2 = "./sygus_debug/" ^ top_nt ^ "_out2.smt2" in

  (* Create sygus input file *)
  let oc = open_out input_filename in
  let ppf = Format.formatter_of_out_channel oc in
  pp_print_ast ppf (ctx, dep_map, ast);
  Format.pp_print_flush ppf ();
  close_out oc;

  (* Call sygus command *)
  let cvc5 = "/home/pirwani/Desktop/cvc5/build/bin/cvc5" in
  let cvc5_2 = "/home/pirwani/Desktop/cvc5-2/build/bin/cvc5" in
  let command = Printf.sprintf "%s --lang=sygus2 %s > %s" cvc5 input_filename output_filename in
  let command2 = Printf.sprintf "%s --lang=sygus2 %s > %s" cvc5_2 input_filename output_filename2 in
  (* Run two versions of sygus in parallel and use results from whichever finishes first *)
  match run_commands command command2 with 
  | Command1 -> 
    let ic = open_in output_filename in
    let len = in_channel_length ic in
    let output = really_input_string ic len in
    close_in ic;
    output
  | Command2 -> 
    let ic = open_in output_filename2 in
    let len = in_channel_length ic in
    let output = really_input_string ic len in
    close_in ic;
    output