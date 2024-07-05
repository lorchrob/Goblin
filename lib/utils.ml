open Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type context = il_type StringMap.t

(* Module state for creating fresh identifiers *)
let k = ref 0

let rec split3 lst =
  match lst with
  | [] -> ([], [], [])
  | (x, y, z) :: t ->
      let (xs, ys, zs) = split3 t in
      (x :: xs, y :: ys, z :: zs)

let mk_fresh_stub_id () = 
  let id = "_stub" ^ (string_of_int !k) ^ "_grammar_element" in 
  k := !k + 1;
  String.uppercase_ascii id

let capture_output: (Format.formatter -> 'a -> unit) -> 'a -> string = 
fun f arg ->
  let buf = Buffer.create 80 in
  let ppf = Format.formatter_of_buffer buf in
  f ppf arg;                (* Call the function with redirected output *)
  Format.pp_print_flush ppf ();  (* Flush the formatter to ensure all output is captured *)
  Buffer.contents buf  (* Retrieve the contents of the buffer as a string *)

let grammar_element_to_string: grammar_element -> string 
= fun grammar_element -> match grammar_element with 
  | Nonterminal nt2 
  | NamedNonterminal (_, nt2) -> nt2
  | StubbedNonterminal (_, stub_id) -> stub_id

let parse: string -> ast 
= fun s ->
  let lexbuf = Lexing.from_string s in 
  Parser.s Lexer.read lexbuf

let parse_sygus: string -> SygusAst.sygus_ast 
= fun s ->
  let lexbuf = Lexing.from_string s in 
  SygusParser.s SygusLexer.read lexbuf

let pp_print_string_map_keys: Format.formatter -> 'a StringMap.t -> unit 
= fun ppf map -> 
  StringMap.iter (fun k _ -> Format.pp_print_string ppf k) map