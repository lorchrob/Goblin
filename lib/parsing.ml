let parse: string -> Ast.ast 
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