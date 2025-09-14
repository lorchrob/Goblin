{
  open SygusParser

  let debug = false

  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keyword_table = mk_hashtbl [
    "define-fun", DEFINEFUN ;
    "top", TOP ;
    "as", AS ;
    "seq", SEQ ;
    "set", SET ; 
    "Set", SETTYPE ;
    "singleton", SINGLETON ;
    "union", UNION ;
    "Seq", CAPSEQ ;
    "Bool", BOOL ;
    "empty", EMPTY ;
    "Unit", UNIT_TYPE ;
    "unit", UNIT ;
    "true", TRUE ;
    "false", FALSE ;
    "Int", INT ;
    "BitVec", BITVEC ;
    "String", STRINGTYPE ;
    "str", STR ;
    "infeasible", INFEASIBLE ;
    "sat", SAT ; 
    "unsat", UNSAT ;
  ] 
}

let white = [' ' '\t']+
let newline = '\r' * '\n'
let digit = ['0'-'9']
let bit = ['0' '1']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '-' '0'-'9']*
let comment = ';' [^ '\n' '\r']* 

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | comment   { read lexbuf }  
  | "-" { Utils.debug_print Format.pp_print_string Format.std_formatter "-"; HYPHEN }
  | "(" { Utils.debug_print Format.pp_print_string Format.std_formatter "("; LPAREN }
  | ")" { Utils.debug_print Format.pp_print_string Format.std_formatter ")"; RPAREN }
  | "." { Utils.debug_print Format.pp_print_string Format.std_formatter "."; DOT } 
  | "_" { Utils.debug_print Format.pp_print_string Format.std_formatter "_"; UNDERSCORE } 
  | "++" { Utils.debug_print Format.pp_print_string Format.std_formatter "++"; PLUSPLUS }
  | "#b" { Utils.debug_print Format.pp_print_string Format.std_formatter "BITS"; read_bits lexbuf }
  |  '"' ([^ '"'] | "\"\"" )* '"' as s   { Utils.debug_print Format.pp_print_string Format.std_formatter (String.sub s 1 (String.length s - 2)); STRCONST (String.sub s 1 (String.length s - 2)) }
  | "$" { Utils.debug_print Format.pp_print_string Format.std_formatter "$"; DOLLAR }
  | "@" { Utils.debug_print Format.pp_print_string Format.std_formatter "@"; AT }
  | int as p { INTEGER (int_of_string p) }
  | id as p {
    try (
      Utils.debug_print Format.pp_print_string Format.std_formatter p; 
      Hashtbl.find keyword_table p
    ) with Not_found -> ID (p)
  }
  | eof { EOF }
  | _ as c { Utils.crash (Printf.sprintf "Unexpected character: %c" c) }

and read_bits = parse
  | bit+ as b { BITS (List.of_seq (String.to_seq b |> Seq.map (fun c -> c = '1'))) }
  | _ { Utils.crash "Invalid bit sequence" }
