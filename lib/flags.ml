let debug = ref false
let only_parse = ref false
let filename = ref None  (* Store the filename *)

let parse_args () = 
  let open Cmdliner in
  let debug_flag =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  let only_parse_flag =
    let doc = "Only parse the input without executing it" in
    Arg.(value & flag & info ["p"; "only-parse"] ~doc)
  in
  let filename_flag =
    let doc = "Specify the input file" in
    Arg.(value & opt (some string) None & info ["f"; "file"] ~doc)
  in
  let set_flags new_debug new_only_parse new_filename = 
    debug := new_debug;
    only_parse := new_only_parse;
    filename := new_filename;
  in
  let term = Term.(const set_flags $ debug_flag $ only_parse_flag $ filename_flag) in
  let info = Cmd.info "sbf" in
  let cmd = Cmd.v info term in
  let _ = Cmd.eval cmd in 
  ()

