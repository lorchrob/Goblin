{
  open Parser

  exception SyntaxError of string

  let debug = false

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
    "ubv_to_int", UBV_TO_INT ; 
    "sbv_to_int", SBV_TO_INT ;
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
    "repeat", REPEAT ;
    "re.++", RE_CONCAT ;
    "re.*", RE_STAR ; 
  ] 
}

let white = [' ' '\t']+
let newline = '\r' * '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let decimal = digit+ '.' digit+   (* e.g. 123.45 *)
             | '.' digit+         (* e.g. .45 *)
             | digit+ '.'         (* e.g. 123. *)
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '-' '.' '+' '*' '0'-'9']*

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | "//" [^ '\n']* '\n' { read lexbuf }
  | ":=" { Utils.debug_print Format.pp_print_string Format.std_formatter ":="; GETS }
  | "::=" { Utils.debug_print Format.pp_print_string Format.std_formatter "::="; PRODUCTION }
  | "::" { Utils.debug_print Format.pp_print_string Format.std_formatter "::"; TYPEANNOT }
  | "|" { Utils.debug_print Format.pp_print_string Format.std_formatter "|"; OPTION }
  | "<" { Utils.debug_print Format.pp_print_string Format.std_formatter "<"; LT }
  | ">" { Utils.debug_print Format.pp_print_string Format.std_formatter ">"; GT }
  | "<=" { Utils.debug_print Format.pp_print_string Format.std_formatter "<="; LTE }
  | ">=" { Utils.debug_print Format.pp_print_string Format.std_formatter ">="; GTE }
  | "=" { Utils.debug_print Format.pp_print_string Format.std_formatter "="; EQ }
  | "=>" { Utils.debug_print Format.pp_print_string Format.std_formatter "=>"; LIMPLIES }
  | "{" { Utils.debug_print Format.pp_print_string Format.std_formatter "{"; LCURLY }
  | "}" { Utils.debug_print Format.pp_print_string Format.std_formatter "}"; RCURLY }
  | "<-" { Utils.debug_print Format.pp_print_string Format.std_formatter "<-"; ASSIGN }
  | "str.++" { Utils.debug_print Format.pp_print_string Format.std_formatter "str.++"; STRCONCAT }
  | "+" { Utils.debug_print Format.pp_print_string Format.std_formatter "+"; PLUS }
  | "[" { Utils.debug_print Format.pp_print_string Format.std_formatter "["; LSQBRACKET }
  | "]" { Utils.debug_print Format.pp_print_string Format.std_formatter "]"; RSQBRACKET }
  | "-" { Utils.debug_print Format.pp_print_string Format.std_formatter "-"; MINUS }
  | "*" { Utils.debug_print Format.pp_print_string Format.std_formatter "*"; TIMES }
  | "div" { Utils.debug_print Format.pp_print_string Format.std_formatter "div"; DIV }
  | "@" { Utils.debug_print Format.pp_print_string Format.std_formatter "@"; AT }
  | "(" { Utils.debug_print Format.pp_print_string Format.std_formatter "("; LPAREN }
  | ")" { Utils.debug_print Format.pp_print_string Format.std_formatter ")"; RPAREN }
  | "," { Utils.debug_print Format.pp_print_string Format.std_formatter ","; COMMA }
  | ";" { Utils.debug_print Format.pp_print_string Format.std_formatter ";"; SEMICOLON }
  | "." { Utils.debug_print Format.pp_print_string Format.std_formatter "."; DOT }
  | "0b" { read_bits lexbuf }
  | "0x" { read_hex lexbuf }
  | '"'[^ '"']*'"' as s   {
    let content = String.sub s 1 (String.length s - 2) in
    Utils.debug_print Format.pp_print_string Format.std_formatter ("\"" ^ content ^ "\" ");
    STRING content
  }
  | int as p {
    Utils.debug_print Format.pp_print_string Format.std_formatter (p);
    INTEGER (int_of_string p)
  }
  | decimal as p {
    Utils.debug_print Format.pp_print_string Format.std_formatter (p);
    DECIMAL (float_of_string p)
  }
  | id as p {
    try (
      Utils.debug_print Format.pp_print_string Format.std_formatter (p);
      Hashtbl.find keyword_table p
    ) with Not_found -> ID (p)
  }
  | eof { Utils.debug_print Format.pp_print_string Format.std_formatter "EOF "; EOF }
  | _ as c { raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c)) }

and read_bits = parse
  | bit+ as b {
      Utils.debug_print Format.pp_print_string Format.std_formatter ("0b" ^ b ^ " ");
      BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1')))
    }
  | _ { Utils.crash "Invalid bit sequence" }

and read_hex = parse
  | ['0'-'9''a'-'f''A'-'F']+ as h {
      Utils.debug_print Format.pp_print_string Format.std_formatter ("0x" ^ h ^ " ");
      let bits =
        h
        |> String.to_seq
        |> Seq.map (fun c ->
             let v =
               if '0' <= c && c <= '9' then Char.code c - Char.code '0'
               else if 'a' <= c && c <= 'f' then 10 + Char.code c - Char.code 'a'
               else 10 + Char.code c - Char.code 'A'
             in
             (* expand nibble to four booleans, MSB first *)
             [ (v land 8) <> 0;
               (v land 4) <> 0;
               (v land 2) <> 0;
               (v land 1) <> 0 ])
        |> List.of_seq
        |> List.flatten
      in
      BITS bits
    }
  | _ { Utils.crash "Invalid hex sequence" }
