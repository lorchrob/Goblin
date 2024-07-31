{
  open SygusParser

  let debug = false

  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keyword_table = mk_hashtbl [
    "define", DEFINE ;
    "fun", FUN ;
    "top", TOP ;
    "as", AS ;
    "seq", SEQ ;
    "Seq", CAPSEQ ;
    "Bool", BOOL ;
    "empty", EMPTY ;
    "unit", UNIT ;
    "true", TRUE ;
    "false", FALSE ;
    "Int", INT ;
    "BitVec", BITVEC ;
    "str", STR ;
  ] 
}

let white = [' ' '\t']+
let newline = '\r' * '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | "-" { Debug.debug_print Format.pp_print_string Format.std_formatter "-"; HYPHEN }
  | "(" { Debug.debug_print Format.pp_print_string Format.std_formatter "("; LPAREN }
  | ")" { Debug.debug_print Format.pp_print_string Format.std_formatter ")"; RPAREN }
  | "." { Debug.debug_print Format.pp_print_string Format.std_formatter "."; DOT } 
  | "_" { Debug.debug_print Format.pp_print_string Format.std_formatter "_"; UNDERSCORE } 
  | "++" { Debug.debug_print Format.pp_print_string Format.std_formatter "++"; PLUSPLUS }
  | "#b" { Debug.debug_print Format.pp_print_string Format.std_formatter "BITS"; read_bits lexbuf }
  | int as p { INTEGER (int_of_string p) }
  | id as p {
    try (
      Debug.debug_print Format.pp_print_string Format.std_formatter p; 
      Hashtbl.find keyword_table p
    ) with Not_found -> ID (p)
  }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Unexpected character: %c" c) }

and read_bits = parse
  | bit+ as b { BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1'))) }
  | _ { failwith "Invalid bit sequence" }