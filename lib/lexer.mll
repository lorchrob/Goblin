{
  open Parser

  exception SyntaxError of string

  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keyword_table = mk_hashtbl [
    "empty_set", EMPTYSET ;
    "member", MEMBER ; 
    "union", UNION ;
    "intersection", INTERSECTION ;
    "singleton", SINGLETON ;
    "Bool", BOOL ;
    "Set", SET ;
    "Int", INT ;
    "Placeholder", PLACEHOLDER ;
    "String", STRINGTYPE ; 
    "BitVector", BITVECTOR ;
    "int_to_bitvector", INTTOBITVECTOR ;
    "BitList", BITLIST ; 
    "length", LENGTH ;
    "str_length", STRLENGTH ;
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
    "bvlt", BVLT ;
    "bvlte", BVLTE ;
    "bvgt", BVGT ; 
    "bvgte", BVGTE ;

    "is_prefix", STRPREFIX ;
    "contains", STRCONTAINS ; 

    "str_to_re", STR_TO_RE ; 
    "str_in_re", STR_IN_RE ; 
    "re_range", RE_RANGE ; 
    "re_union", RE_UNION ; 
    "re_star", RE_STAR ; 
    "re_concat", RE_CONCAT ;
  ] 
}

let white = [' ' '\t']+
let newline = '\r' * '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '-' '0'-'9']*

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | "//" [^ '\n']* '\n' { read lexbuf }
  | "::=" { PRODUCTION }
  | "::" {TYPEANNOT}
  | "|" { OPTION }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "=" { EQ }
  | "=>" { LIMPLIES }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "<-" { ASSIGN }
  (* | "->" { ARROW } *)
  | "++" { STRCONCAT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "." { DOT }
  | "0b" { read_bits lexbuf }
  | '"'[^ '"']*'"' as s   { STRING (String.sub s 1 (String.length s - 2)) }
  | int as p { INTEGER (int_of_string p) }
  | id as p {
    try (
      Utils.debug_print Format.pp_print_string Format.std_formatter p; (* switch to true for more debug output *)
      Hashtbl.find keyword_table p
    ) with Not_found -> ID (p)
  }
  | eof { EOF }
  | _ as c { raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c)) }

and read_bits = parse
  | bit+ as b { BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1'))) }
  | _ { Utils.crash "Invalid bit sequence" }
