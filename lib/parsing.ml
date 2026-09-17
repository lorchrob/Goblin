let parse: string -> Ast.ast 
= fun s ->
  let lexbuf = Lexing.from_string s in
  try
    Parser.s Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg -> 
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at %s, line %d, column %d: %s\n"
        (!Flags.filename |> Option.get)
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg;
      exit 1
  | Parser.Error  ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at %s, line %d, column %d\n"
        (!Flags.filename |> Option.get)
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1

(* Helper function to format position information *)
let format_position (pos : Lexing.position) : string =
  Printf.sprintf "line %d, column %d" 
    pos.Lexing.pos_lnum 
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let parse_solver: string -> Ast.ast -> (SolverAst.solver_ast, string) result
= fun s _ast ->
  let lexbuf = Lexing.from_string s in
  let error_message () =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "Syntax error at %s, %s" 
    (!Flags.filename |> Option.get)
    (format_position pos)
  in
  let solver_ast =
    try
      Ok (SolverParser.s SolverLexer.read lexbuf)
    with
    | SolverParser.Error ->  
        Error (error_message ())
    | e ->
        Error (Printexc.to_string e)
  in
  match solver_ast with 
  | Error e -> print_endline e; solver_ast
  | Ok _ -> solver_ast
