{
  open Parser

  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keyword_table = mk_hashtbl [
    "Bool", BOOL ;
    "Int", INT ;
    "BitVector", BITVECTOR ;
    "bitvector", BITVECTOR;
    "BitList", BITLIST ; 
    "MachineInt", MACHINEINT ;
    "case", CASE ;
    "of", OF ;
    "to", TO ;
    "length", LENGTH ;
    "land", LAND ;
    "lor", LOR ;
    "lxor", LXOR ;
    "lnot", LNOT ;
    "bvand", BVAND ;
    "bvor", BVOR ; 
    "bvxor", BVXOR ; 
    "bvnot", BVNOT ;
    "true", TRUE ; 
    "false", FALSE ;
  ] 
}

let white = [' ' '\t']+
let newline = '\r'* '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | "::=" { PRODUCTION }
  | "::" {TYPEANNOT}
  | "|" { OPTION }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "<=" { LTE }
  | ">=" { GTE }
  | "=" { EQ }
  | "=>" { LIMPLIES }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "<-" { ASSIGN }
  | "->" { ARROW }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "(" { RPAREN }
  | ")" { LPAREN }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "." { DOT }
  | "_" { UNDERSCORE }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "0b" { read_bits lexbuf }
  | int as p { INTEGER (int_of_string p) }
  | id as p {
    try Hashtbl.find keyword_table p with Not_found -> ID (p)
  }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %c" c) }

and read_bits = parse
  | bit+ as b { BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1'))) }
  | _ { failwith "Invalid bit sequence" }