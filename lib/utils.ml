open Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type context = il_type StringMap.t

let parse: string -> ast 
= fun s ->
  let lexbuf = Lexing.from_string s in 
  Parser.s Lexer.read lexbuf

let parse_sygus: string -> Ast.ast -> (SygusAst.sygus_ast, string) result
= fun s ast ->
  let lexbuf = Lexing.from_string s in 
  let sygus_ast = 
    try 
      Ok (SygusParser.s SygusLexer.read lexbuf) 
    with e ->
      Error (Printexc.to_string e)
  in 
  match ast, sygus_ast with 
  | ProdRule _ :: _, _ -> sygus_ast 
  (* Sygus files with top-level type annotations lose their constructor name *)
  | TypeAnnotation (nt, _, _) :: _, Ok sygus_ast -> 
    let constructor = String.lowercase_ascii nt ^ "_con0" in
    Ok (SygusAst.Node (constructor, [sygus_ast]))
  | _, Error e -> Error e
  | [], _ -> assert false


(* Module state for creating fresh identifiers *)
let k = ref 0

let rec split3 lst =
  match lst with
  | [] -> ([], [], [])
  | (x, y, z) :: t ->
      let (xs, ys, zs) = split3 t in
      (x :: xs, y :: ys, z :: zs)

let mk_fresh_stub_id id = 
  let id = "_stub" ^ (string_of_int !k) ^ "_" ^ id in 
  k := !k + 1;
  String.uppercase_ascii id

let il_int_to_bitvector: int -> int -> expr 
= fun length n ->
  if n >= (1 lsl length) then
    failwith ("Tried to cast integer " ^ string_of_int n ^ " to BitVector of inadequate width " ^ string_of_int length)
  else
    let rec to_bits acc len n =
      if len = 0 then acc
      else
        let bit = (n land 1) = 1 in
        to_bits (bit :: acc) (len - 1) (n lsr 1)
    in
    let bits = to_bits [] length n in 
    BVConst (length, bits)
    
let find_index predicate lst =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: xs -> if predicate x then i else aux (i + 1) xs
  in
  aux 0 lst

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

let pp_print_string_map_keys: Format.formatter -> 'a StringMap.t -> unit 
= fun ppf map -> 
  StringMap.iter (fun k _ -> Format.pp_print_string ppf k) map