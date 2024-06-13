open Ast

let capture_output: ('a -> unit) -> 'a -> identifier = 
fun f arg ->
  let buf = Buffer.create 80 in
  let ppf = Format.formatter_of_buffer buf in
  f arg;                (* Call the function with redirected output *)
  Format.pp_print_flush ppf ();  (* Flush the formatter to ensure all output is captured *)
  Buffer.contents buf  (* Retrieve the contents of the buffer as a string *)

let parse: string -> ast 
= fun s ->
  let lexbuf = Lexing.from_string s in 
  Parser.s Lexer.read lexbuf