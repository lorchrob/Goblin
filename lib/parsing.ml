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

(* Positions here index the solver's reply, not the user's grammar, so neither the
   grammar's filename nor its coordinates belong in these messages. The caller
   reports the failure; returning it silently keeps it from being printed twice. *)
let parse_solver: string -> Ast.ast -> (SolverAst.solver_ast, string) result
= fun s _ast ->
  let lexbuf = Lexing.from_string s in
  try
    Ok (SolverParser.s SolverLexer.read lexbuf)
  with
  | SolverParser.Error ->
      Error (Printf.sprintf "syntax error at %s of the reply"
        (format_position lexbuf.lex_curr_p))
  | Failure msg -> Error msg
  | e -> Error (Printexc.to_string e)
