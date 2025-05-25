open Sbf

let () = 
  Flags.parse_args ();

  if !Flags.daniyal then 
    let commit_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit.txt") in
    let confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/confirm.txt") in
    let commit_confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit-confirm.txt") in
    GrammarFuzzing.runFuzzer [commit_grammar; confirm_grammar; commit_confirm_grammar;]

  else
    let filename = match !Flags.filename with 
    | Some filename -> filename 
    | None -> Utils.error "You must specify an input filename with --file <filename>"
    in
     
    let _ = Pipeline.main_pipeline filename in

    ()

    (* let input = "./test/test_cases/test_dot_notation" in
    let ast = Parsing.parse (Utils.read_file input) in
    let sygus_ast, _ = Pipeline.main_pipeline input in
    Format.fprintf Format.std_formatter "AST: %a\n" Ast.pp_print_ast ast;
    Format.fprintf Format.std_formatter "SyGuS AST: %a\n" SygusAst.pp_print_sygus_ast sygus_ast;
    let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
    match output with
    | Ok _ -> ()  
    | Error msg -> Utils.crash msg *)