type engine = 
  | SygusDac 
  | DpllDac
  | DpllMono

let debug = ref false
let no_warnings = ref true
let only_parse = ref false
let daniyal = ref false
let filename = ref None
let selected_engine = ref None

let parse_args () = 
  let open Cmdliner in

  (* Convert string to engine variant *)
  let engine_conv =
    let parse = function
    | "sygus_dac" -> Ok SygusDac
    | "dpll_dac" -> Ok DpllDac
    | "dpll_mono" -> Ok DpllMono
    | s -> Error (`Msg ("Invalid engine: " ^ s))
    in
    let print fmt = function
    | SygusDac -> Format.fprintf fmt "sygus_dac"
    | DpllDac -> Format.fprintf fmt "dpll_dac"
    | DpllMono -> Format.fprintf fmt "dpll_mono"
    in
    Arg.conv (parse, print)
  in

  let debug_flag =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in

  let no_warnings_flag =
    let doc = "Disable warnings" in
    Arg.(value & flag & info ["n"; "no-warnings"] ~doc)
  in

  let only_parse_flag =
    let doc = "Only parse the input without executing it" in
    Arg.(value & flag & info ["p"; "only-parse"] ~doc)
  in

  let daniyal_flag =
    let doc = "Activate daniyal mode" in
    Arg.(value & flag & info ["a"; "daniyal"] ~doc)
  in

  let filename_flag =
    let doc = "Specify the input file" in
    Arg.(value & opt (some string) None & info ["f"; "file"] ~doc)
  in

  let engine_flag =
    let doc = "Select a single engine to use (dpll_mono, dpll_dac, or sygus_dac)" in
    Arg.(value & opt (some engine_conv) None & info ["e"; "engine"] ~docv:"ENGINE" ~doc)
  in

  let set_flags new_debug new_no_warnings new_only_parse new_daniyal new_filename new_engine =
    Format.pp_print_flush Format.std_formatter ();
    debug := new_debug;
    no_warnings := new_no_warnings;
    only_parse := new_only_parse;
    daniyal := new_daniyal;
    filename := new_filename;
    selected_engine := new_engine;
  in

  let term =
    Term.(const set_flags
          $ debug_flag
          $ no_warnings_flag
          $ only_parse_flag
          $ daniyal_flag
          $ filename_flag
          $ engine_flag)
  in

  let info = Cmd.info "sbf" in
  let cmd = Cmd.v info term in
  let _ = Cmd.eval cmd in 
  ()
