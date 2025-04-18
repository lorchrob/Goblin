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
    | None -> Utils.crash "You must specify an input filename with --file <filename>"
    in
    
    let _ = Pipeline.main_pipeline filename in

    ()