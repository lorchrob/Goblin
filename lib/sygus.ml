open Ast

(* let pp_print_datatypes = assert false 

let pp_print_grammar = assert false 

let pp_print_semantic_constraints = assert false  *)

let pp_print_ast: Format.formatter -> ast -> unit 
= fun ppf _ -> 
  Format.fprintf ppf "(set-logic ALL)\n";
  Format.fprintf ppf "(check-synth)"