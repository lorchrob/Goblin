open Ast

(* Module state for creating fresh identifiers *)
let i = ref 0

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

let pp_print_datatypes: Format.formatter -> ast -> unit 
= fun ppf ast -> 
  List.iter (fun element -> match element with 
  | TypeAnnotation (nt, ty, _) -> 
    Format.fprintf ppf 
    "(declare-datatype %s (\n\t(%s (%s %a))\n))"
    (String.uppercase_ascii nt)  
    (fresh_ident ())
    (fresh_ident ())
    pp_print_ty ty;
    Lib.print_newline ppf;
  | ProdRule _ -> ()
  ) ast 

  (* (sae_packet SAE_PACKET) 
  (auth_algo AUTH_ALGO) 
  (status_code STATUS_CODE)
  (bitvec_16 (_ BitVec 16))  *)
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
= fun _ _ -> ()


let pp_print_grammar: Format.formatter -> ast -> unit 
= fun ppf ast -> 
  Format.fprintf ppf 
  "(synth-fun top () TOP\n; declare nonterminals\n(\n%a)\n; grammar rules\n(\n%a\n)\n)"
   pp_print_nt_decs ast 
   pp_print_rules ast


let pp_print_ast: Format.formatter -> ast -> unit 
= fun ppf ast -> 
  Format.fprintf ppf "(set-logic ALL)";

  Lib.print_newline ppf;
  Lib.print_newline ppf;

  pp_print_datatypes ppf ast;

  Lib.print_newline ppf;

  pp_print_grammar ppf ast;
  
  Lib.print_newline ppf;

  Format.fprintf ppf "(check-synth)";

  Lib.print_newline ppf;