let debug = ref false
let only_parse = ref false

let parse_args () = 
  let open Cmdliner in
  let debug_flag =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  let only_parse_flag =
    let doc = "Only parse the input without executing it" in
    Arg.(value & flag & info ["p"; "only_parse"] ~doc)
  in
  let set_flags (new_debug: bool) (new_only_parse: bool) = 
    debug := new_debug;
    only_parse := new_only_parse
  in
  let term = Term.(const set_flags $ debug_flag $ only_parse_flag) in
  let info = Cmd.info "sbf" in
  let cmd = Cmd.v info term in
  let _ = Cmd.eval cmd in 
  ()

let debug_print pp formatter value =
  if !debug then
    (pp formatter value; 
     Format.pp_print_flush formatter ();)
  else
    Format.ifprintf formatter "%a" pp value