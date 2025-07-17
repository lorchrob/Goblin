(*
  TODO: 
    1. Dep map should map nt and prod rule RHS to expr, not just nt 
*)

open Sbf

let () = 
  Flags.parse_args ();

  if !Flags.daniyal then 
    let commit_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit.txt") in
    let confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/confirm.txt") in
    let commit_confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit-confirm.txt") in
    GrammarFuzzing.runFuzzer [commit_grammar; confirm_grammar; commit_confirm_grammar;]
  else if !Flags.analysis <> "" then 
    AnalyzeGoblinOutput.evaluate () 
  else 
    let filename = match !Flags.filename with 
    | Some filename -> filename 
    | None -> Utils.error "You must specify an input filename with --file <filename>"
    in

    if !Flags.dump_clp then 
      let ppf = Format.std_formatter in
      let input_string = Utils.read_file filename in 
      let ast = Parsing.parse input_string in   
      let clp_program = ClpTranslator.clp_program_of_ast ast in 
      ClpTranslator.pp_print_clp_program ppf clp_program
    else
      let _ = Pipeline.main_pipeline filename in
      ()
