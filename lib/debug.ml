let debug = ref false

let parse_args () = 
  let open Cmdliner in
  let debug_flag =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  let set_debug (new_debug: bool) = 
    debug := new_debug
  in
  let term = Term.(const set_debug $ debug_flag) in
  let info = Cmd.info "sbf" in
  let cmd = Cmd.v info term in
  let _ = Cmd.eval cmd in 
  ()

let debug_print pp formatter value =
  if !debug then
    pp formatter value
  else
    Format.ifprintf formatter "%a" pp value