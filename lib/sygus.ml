open Ast

(* Module state for creating fresh identifiers *)
let i = ref 0

module TC = TypeChecker

let fresh_ident: unit -> string 
= fun () ->
  let id = "id" ^ string_of_int !i in 
  i := !i + 1;
  id

let pp_print_ty: Format.formatter -> il_type -> unit 
= fun ppf ty -> match ty with 
| Int -> Format.fprintf ppf "Int"
| Bool -> Format.fprintf ppf "Bool"
| BitVector len -> Format.fprintf ppf "(_ BitVec %d)" len
| BitList -> Format.fprintf ppf "(Seq Bool)"
| MachineInt width -> Format.fprintf ppf "(_ BitVec %d)" (Lib.pow 2 width)

let pp_print_constructor: TC.context -> Format.formatter -> grammar_element -> unit 
= fun ctx ppf ge -> match ge with 
| Nonterminal nt 
| NamedNonterminal (_, nt) -> 
  let d_str, ty_str = match TC.StringMap.find_opt nt ctx with 
  | None -> fresh_ident (), String.uppercase_ascii nt
  | Some ty -> String.lowercase_ascii nt, Utils.capture_output pp_print_ty ty
  in
  Format.fprintf ppf "(%s %s)"
  d_str
  (String.uppercase_ascii ty_str)

let pp_print_datatypes: Format.formatter -> TC.context -> ast -> unit 
= fun ppf ctx ast -> 
  List.iter (fun element -> match element with 
  | TypeAnnotation _ -> ()
  | ProdRule (nt, ges, _) -> 
    Format.fprintf ppf "(declare-datatype %s (\n\t(%s %a)\n))"
    (String.uppercase_ascii nt)
    (fresh_ident ())
    (Lib.pp_print_list (pp_print_constructor ctx) " ") ges;
    Lib.print_newline ppf;
  ) ast 

let pp_print_nt_decs: Format.formatter -> ast -> unit 
= fun ppf ast -> List.iter (fun element -> match element with 
| ProdRule (nt, _, _) -> 
  Format.fprintf ppf "\t(%s %s)\n"
  (String.lowercase_ascii nt) 
  (String.uppercase_ascii nt) 
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a)\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
) ast

  (* (sae_packet SAE_PACKET ((R3 auth_algo status_code)))
      (auth_algo AUTH_ALGO ((R2 bitvec_16)))
      (status_code STATUS_CODE ((R1 bitvec_16)))
      (bitvec_16 (_ BitVec 16) ((Constant (_ BitVec 16)))) *)
let pp_print_rules: Format.formatter -> ast -> unit 
= fun ppf ast -> List.iter (fun element -> match element with 
| ProdRule (nt, ges, _) -> 
  let ges = List.map Utils.grammar_element_to_string ges in 
  let ges = List.map String.lowercase_ascii ges in
  Format.fprintf ppf "\t(%s %s ((idx %a)))\n"
  (String.lowercase_ascii nt) 
  (String.uppercase_ascii nt) 
  (Lib.pp_print_list Format.pp_print_string " ") ges
| TypeAnnotation (nt, ty, _) -> 
  Format.fprintf ppf "\t(%s %a ((Constant %a)))\n"
  (String.lowercase_ascii nt) 
  pp_print_ty ty
  pp_print_ty ty
) ast


let pp_print_grammar: Format.formatter -> ast -> unit 
= fun ppf ast -> 
  Format.fprintf ppf 
  "(synth-fun top () TOP\n; declare nonterminals\n(\n%a)\n; grammar rules\n(\n%a\n)\n)"
   pp_print_nt_decs ast 
   pp_print_rules ast


let pp_print_ast: Format.formatter -> TC.context -> ast -> unit 
= fun ppf ctx ast -> 
  Format.fprintf ppf "(set-logic ALL)";

  Lib.print_newline ppf;
  Lib.print_newline ppf;

  pp_print_datatypes ppf ctx ast;

  Lib.print_newline ppf;

  pp_print_grammar ppf ast;
  
  Lib.print_newline ppf;

  Format.fprintf ppf "(check-synth)";

  Lib.print_newline ppf;