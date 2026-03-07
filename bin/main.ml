(*
  TODO: 
    1. Dep map should map nt and prod rule RHS to expr, not just nt 
*)

open Goblin 

let () = 
  Flags.parse_args ();

  try 
    let filename = match !Flags.filename with 
    | Some filename -> filename 
    | None -> Utils.error_no_pos "You must specify an input file with --file <file_path>"
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
  | Failure s -> 
    Format.eprintf "%s@." s; 
    exit 1
