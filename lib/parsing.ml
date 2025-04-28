let parse: string -> Ast.ast 
= fun s ->
  let lexbuf = Lexing.from_string s in
  try
    Parser.s Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg -> 
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d: %s\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg;
      exit 1
  | Parser.Error  ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1

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
  | ProdRule _ :: _, Error e -> print_endline e; sygus_ast
  | ProdRule _ :: _, Ok _ -> sygus_ast 
  (* Sygus files with top-level type annotations lose their constructor name *)
  | TypeAnnotation (nt, _, _) :: _, Ok sygus_ast -> 
    let constructor = String.lowercase_ascii nt ^ "_con0" in
    Ok (SygusAst.Node (constructor, [sygus_ast]))
  | _, Error e -> print_endline e; sygus_ast
  | _, Ok _ -> sygus_ast