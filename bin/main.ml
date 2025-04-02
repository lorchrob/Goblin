open Sbf

let () = 
  Flags.parse_args ();

  let filename = match !Flags.filename with 
  | Some filename -> filename 
  | None -> Utils.crash "You must specify an input filename with --file <filename>"
  in
  
  let _ = Pipeline.main_pipeline filename in

  ()