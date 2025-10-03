(*
  TODO: 
    1. Dep map should map nt and prod rule RHS to expr, not just nt 
*)

open Goblin 

let () = 
  Flags.parse_args ();

  try 
    if !Flags.saecred then 
      let commit_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit.txt") in
      let confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/confirm.txt") in
      let commit_confirm_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/commit-confirm.txt") in
      let eapol_1_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/eapol_1.txt") in
      let eapol_2_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/eapol_2.txt") in
      let eapol_3_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/eapol_3.txt") in
      let eapol_4_grammar = Parsing.parse (GrammarFuzzing.read_grammar "bin/eapol_4.txt") in
      GrammarFuzzing.runFuzzer [commit_grammar; confirm_grammar; commit_confirm_grammar;eapol_1_grammar;eapol_2_grammar;eapol_3_grammar;eapol_4_grammar;]
    else if !Flags.analysis <> "" then 
      AnalyzeGoblinOutput.evaluate () 
    else 
      let filename = match !Flags.filename with 
      | Some filename -> filename 
      | None -> Utils.error_no_pos "You must specify an input filename with --file <filename>"
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
     with
  | exn -> 
      Format.eprintf "(%s) %s@." (Option.get !Flags.filename) (Printexc.to_string exn);
      exit 1
