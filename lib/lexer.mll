{
  open Parser

  exception SyntaxError of string

  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keyword_table = mk_hashtbl [
    "set.empty", EMPTYSET ;
    "set.member", MEMBER ; 
    "set.union", UNION ;
    "set.inter", INTERSECTION ;
    "set.singleton", SINGLETON ;
    "Bool", BOOL ;
    "Set", SET ;
    "List", LIST ; 
    "Int", INT ;
    "Unit", UNIT ;
    "String", STRINGTYPE ; 
    "BitVec", BITVECTOR ;
    "int_to_bv", INTTOBITVECTOR ;
    "BitList", BITLIST ; 
    "seq.len", SEQLENGTH ; 
    "length", LENGTH ;
    "str.len", STRLENGTH ;
    "and", LAND ;
    "or", LOR ;
    "xor", LXOR ;
    "not", LNOT ;
    "mod", MOD ; 
    "bvand", BVAND ;
    "bvor", BVOR ; 
    "bvxor", BVXOR ; 
    "bvnot", BVNOT ;
    "true", TRUE ; 
    "false", FALSE ;
    "bvult", BVLT ;
    "bvulte", BVLTE ;
    "bvugt", BVGT ; 
    "bvugte", BVGTE ;
    "str.prefixof", STRPREFIX ;
    "str.contains", STRCONTAINS ; 
    "str.to_re", STR_TO_RE ; 
    "str.in_re", STR_IN_RE ; 
    "re.range", RE_RANGE ; 
    "re.union", RE_UNION ; 
    "re.*", RE_STAR ; 
    "re.++", RE_CONCAT ;
  ] 
}

let white = [' ' '\t']+
let newline = '\r' * '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '-' '.' '+' '*' '0'-'9']*

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
  | "str.++" { STRCONCAT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "div" { DIV }
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
