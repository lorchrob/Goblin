open Sbf

let () = 
  Flags.parse_args ();

  if !Flags.daniyal then 
    let commit_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit.txt") in
    let confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/confirm.txt") in
    let commit_confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit-confirm.txt") in
    GrammarFuzzing.runFuzzer [commit_grammar; confirm_grammar; commit_confirm_grammar;]

  else
    let filename = "./test/test_cases/test_check_sygus_ast_3" in
    let input = Utils.read_file filename in 
    let ast = Parsing.parse input in 
    let sygus_ast = SygusAst.Node (("A", None), [SygusAst.Node (("B", None), [SygusAst.Node (("G", None), [SygusAst.IntLeaf (2)])]); SygusAst.Node (("C", None), [SygusAst.Node (("G", None), [SygusAst.IntLeaf (1)])])]) in
    let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
    match output with
    | Ok _ -> failwith "Expected failure"
    | Error msg -> print_endline msg

    (* let input = "./test/test_cases/test_dot_notation" in
    let ast = Parsing.parse (Utils.read_file input) in
    let sygus_ast, _ = Pipeline.main_pipeline input in
    Format.fprintf Format.std_formatter "AST: %a\n" Ast.pp_print_ast ast;
    Format.fprintf Format.std_formatter "SyGuS AST: %a\n" SygusAst.pp_print_sygus_ast sygus_ast;
    let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
    match output with
    | Ok _ -> ()  
    | Error msg -> Utils.crash msg *)