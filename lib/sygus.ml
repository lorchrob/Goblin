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
(*!! Strings should only be present for dependency computations. 
   If it's here, it is a grammar element that would be pruned anyway. *)
| String -> Format.fprintf ppf "Int"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| MachineInt width -> Format.fprintf ppf "(_ BitVec %d)" (Lib.pow 2 width)
| ADT _ -> failwith "Internal error: sygus.ml (pp_print_ty)"

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
  | SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
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
| BVXor -> failwith "BitVector xor is not supported"
| LAnd -> Format.fprintf ppf "and"
| LOr -> Format.fprintf ppf "or"
| LXor -> Format.fprintf ppf "xor"
| LImplies ->Format.fprintf ppf "=>"
| Plus -> Format.fprintf ppf "+"
| Minus -> Format.fprintf ppf "-"
| Times -> Format.fprintf ppf "*"
| Div -> Format.fprintf ppf "/"

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
| SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
) dep_map

let rec pp_print_match: Format.formatter -> TC.context -> string -> A.case list -> unit 
= fun ppf ctx nt cases ->
  let _options, exprs = List.split cases in
  let rules = match StringMap.find nt ctx with 
  | ADT rules -> rules 
  | _ -> failwith "Internal error: sygus.ml (pp_print_match)" 
  in
  let rules = List.mapi (fun i rule -> (i, rule)) rules in
  let rules = List.combine rules exprs in
  Format.fprintf ppf "(match %s (
    %a
  ))"
  (String.lowercase_ascii nt)
  (Lib.pp_print_list (fun ppf -> Format.fprintf ppf "(%a)" (pp_print_option ctx nt)) " ") rules

and pp_print_option: TC.context -> string -> Format.formatter -> ((int * (string list)) * A.expr) -> unit 
= fun ctx nt ppf ((i, options), expr) -> 
  let options = List.map String.lowercase_ascii options in
  Format.fprintf ppf "(%s_con%d %a) %a"
  (String.lowercase_ascii nt)
  i
  (Lib.pp_print_list Format.pp_print_string " ") options
  (pp_print_expr ctx) expr

and pp_print_expr: TC.context -> Format.formatter -> A.expr -> unit 
= fun ctx ppf expr -> match expr with 
| NTExpr [nt] -> Format.pp_print_string ppf (String.lowercase_ascii nt)
| A.Match (nt, cases)  -> pp_print_match ppf ctx nt cases
| NTExpr _ -> assert false (* pp_print_match ppf ctx nts *)
| BinOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
    pp_print_binop op 
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
| CompOp (expr1, BVLt, expr2) -> 
  Format.fprintf ppf "(bvult %a %a)"
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
| CompOp (expr1, BVLte, expr2) -> 
  Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
| CompOp (expr1, BVGt, expr2) -> 
  Format.fprintf ppf "(bvult %a %a)"
    (pp_print_expr ctx) expr2
    (pp_print_expr ctx) expr1 
| CompOp (expr1, BVGte, expr2) -> 
  Format.fprintf ppf "(or (bvult %a %a) (= %a %a))"
    (pp_print_expr ctx) expr2
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
    (pp_print_expr ctx) expr1 
| CompOp (expr1, op, expr2) -> 
  Format.fprintf ppf "(%a %a %a)"
    pp_print_compop op 
    (pp_print_expr ctx) expr1 
    (pp_print_expr ctx) expr2
| UnOp (op, expr) -> 
  Format.fprintf ppf "(%a %a)"
    pp_print_unop op 
    (pp_print_expr ctx) expr
| Length expr -> 
  Format.fprintf ppf "(seq.len %a)"
    (pp_print_expr ctx) expr
| BVConst (_, bits) -> 
  let bits = List.map Bool.to_int bits in
  Format.fprintf ppf "#b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
| BConst b ->  Format.fprintf ppf "%b" b
| IntConst i -> Format.fprintf ppf "%d" i
| StrConst _ -> failwith "Error: String constants can only be in dependencies (of the form 'nonterminal <- string_literal')"
| BLConst _ -> failwith "BitList literals not yet fully supported"
| BVCast _ -> failwith "Integer to bitvector casts in semantic constraints that aren't preprocessable are not supported"

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

let pp_print_constraints_rhs: TC.context -> string -> Format.formatter -> A.prod_rule_rhs * int -> unit
= fun ctx nt ppf (rhs, idx) -> match rhs with 
| StubbedRhs _ -> Format.fprintf ppf "2STUB\n"
| Rhs (ges, scs) ->
  let 
    ges = List.map Utils.grammar_element_to_string ges |> List.map String.lowercase_ascii 
  in  
  let exprs = List.filter_map (fun sc -> match sc with 
  | A.SyGuSExpr expr -> Some expr 
  | _ -> None
  ) scs in
  if List.length exprs > 1 then
    Format.fprintf ppf "((%s %a)\n\t\t (and %a))"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
      (Lib.pp_print_list (pp_print_expr ctx) " ") exprs
  else if List.length exprs = 1 then 
    Format.fprintf ppf "((%s %a)\n\t\t %a)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
      (Lib.pp_print_list (pp_print_expr ctx) " ") exprs 
  else 
    Format.fprintf ppf "((%s %a)\n\t\t true)"
    ((String.lowercase_ascii nt) ^ "_con" ^ (string_of_int idx)) 
      (Lib.pp_print_list Format.pp_print_string " ") ges
  
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
| [] -> failwith "Input grammar must have at least one production rule or type annotation"
| A.ProdRule (nt, rhss) :: _ -> 
  if List.exists (fun rhs -> match rhs with | A.Rhs (_, _ :: _) -> true | _ -> false) rhss then
  let rhss = List.mapi (fun i rhs -> (rhs, i)) rhss in
  pp_print_semantic_constraints_prod_rule ctx ppf nt rhss
| TypeAnnotation (nt, ty, scs) :: _ -> 
  List.iter (pp_print_semantic_constraint_ty_annot ctx ppf nt ty) scs

let pp_print_rhs: string -> Format.formatter -> A.prod_rule_rhs * int -> unit
= fun nt ppf (rhs, idx) -> match rhs with 
| A.Rhs (ges, _) ->
  let ges = List.map Utils.grammar_element_to_string ges in 
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
  | SyGuSExpr _ -> failwith "Internal error: dependency map contains a SyGuSExpr"
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

let find_command_in_path cmd =
  match Sys.getenv_opt "PATH" with
  | None -> failwith "$PATH is not set"
  | Some path ->
      let paths = String.split_on_char ':' path in
      let rec find_in_paths = function
        | [] -> failwith (cmd ^ " not found in $PATH")
        | dir :: rest ->
            let full_path = Filename.concat dir cmd in
            if Sys.file_exists full_path && Sys.is_directory full_path = false then
              full_path
            else
              find_in_paths rest
      in
      find_in_paths paths

let call_sygus : TC.context -> Ast.semantic_constraint Utils.StringMap.t -> A.ast -> string =
fun ctx dep_map ast ->
  let top_nt = match ast with
  | ProdRule (nt, _) :: _ -> nt
  | TypeAnnotation (nt, _, _) :: _ -> nt
  | _ -> assert false
  in
  ignore (Unix.system "mkdir sygus_debug 2> /dev/null");
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
  let cvc5 = find_command_in_path "cvc5" in
  let cvc5_2 = match Sys.getenv_opt "PATH_TO_SECOND_CVC5" with 
  | Some path -> path 
  | None -> print_endline "Proceeding without a second cvc5 config"; cvc5 
  in
  let command = Printf.sprintf "timeout 3 %s --lang=sygus2 --dag-thresh=0 %s > %s" cvc5 input_filename output_filename in
  let command2 = Printf.sprintf "timeout 3 %s --lang=sygus2 --dag-thresh=0 %s > %s" cvc5_2 input_filename output_filename2 in
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