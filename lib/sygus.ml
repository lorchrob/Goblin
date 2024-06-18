open Ast

let pp_print_ast: Format.formatter -> ast -> unit 
= fun ppf _ -> 
  Format.fprintf ppf "(set-logic ALL)\n"