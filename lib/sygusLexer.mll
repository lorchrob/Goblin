{
  open SygusParser

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
  | "-" { print_endline "-"; HYPHEN }
  | "(" { print_endline "("; LPAREN }
  | ")" { print_endline ")"; RPAREN }
  | "." { print_endline "."; DOT } 
  | "_" { print_endline "_"; UNDERSCORE } 
  | "++" { print_endline "++"; PLUSPLUS }
  | "#b" { print_endline "BITS"; read_bits lexbuf }
  | int as p { INTEGER (int_of_string p) }
  | id as p {
    try (print_endline p; Hashtbl.find keyword_table p) with Not_found -> ID (p)
  }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Unexpected character: %c" c) }

and read_bits = parse
  | bit+ as b { BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1'))) }
  | _ { failwith "Invalid bit sequence" }