(*
  TODO: 
    * Flags for initial depth limit and restart rate 
    * Flag for which SMT solver to use 
*)

type engine = 
  | SygusDac 
  | DpllDac
  | DpllMono
  | MixedDac
  | Race

type output_format = 
  | SExpression
  | Hex 
  | HexPacked

let debug = ref false
let no_warnings = ref true
let only_parse = ref false
let show_winner = ref false
let dump_clp = ref false
let multiple_solutions = ref false
let saecred = ref false
let analysis = ref "" 
let filename = ref None
let selected_engine = ref DpllMono
let output_format = ref SExpression 
let num_solutions = ref (-1) 
let starting_depth_limit = ref 5 
let restart_rate = ref 10000 
let sols_per_iter = ref 10 
let seed = ref None

let parse_args () = 
  let open Cmdliner in

  (* Convert string to engine variant *)
  let engine_conv =
    let parse = function
    | "sygus_dac" -> Ok SygusDac
    | "dpll_dac" -> Ok DpllDac
    | "dpll_mono" -> Ok DpllMono
    | "mixed_dac" -> Ok MixedDac
    | "race" -> Ok Race
    | s -> Error (`Msg ("Invalid engine: " ^ s))
    in
    let print ppf = function
    | SygusDac -> Format.fprintf ppf "sygus_dac"
    | DpllDac -> Format.fprintf ppf "dpll_dac"
    | DpllMono -> Format.fprintf ppf "dpll_mono"
    | MixedDac -> Format.fprintf ppf "mixed_dac"
    | Race -> Format.fprintf ppf "race"
    in
    Arg.conv (parse, print)
  in

  (* Convert string to output format variant *)
  let output_conv =
    let parse = function
    | "s-expression" -> Ok SExpression
    | "hex" -> Ok Hex 
    | "hex-packed" -> Ok HexPacked
    | s -> Error (`Msg ("Invalid output format: " ^ s))
    in
    let print ppf = function
    | SExpression -> Format.fprintf ppf "s-expression"
    | Hex -> Format.fprintf ppf "hex"
    | HexPacked -> Format.fprintf ppf "hex-packed"
    in
    Arg.conv (parse, print)
  in

  let debug_flag =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["debug"] ~doc)
  in

  let no_warnings_flag =
    let doc = "Disable warnings" in
    Arg.(value & flag & info ["no-warnings"] ~doc)
  in

  let only_parse_flag =
    let doc = "Only parse the input without executing it" in
    Arg.(value & flag & info ["only-parse"] ~doc)
  in

  let show_winner_flag =
    let doc = "Show which engine won the race" in
    Arg.(value & flag & info ["show-winner"] ~doc)
  in

  let dump_clp_flag =
    let doc = "Translate the input to a CLP(T) problem" in
    Arg.(value & flag & info ["dump-clp"] ~doc)
  in

  let multiple_solutions_flag = 
    let doc = "Loop and produce multiple solutions (only with --engine dpll_mono)" in 
    Arg.(value & flag & info ["multiple-solutions"] ~doc) 
  in

  let saecred_flag =
    let doc = "Activate saecred mode" in
    Arg.(value & flag & info ["saecred"] ~doc)
  in

  let analysis_flag = 
    let doc = "Post analysis mode for experimental evaluation (--analysis {csv, xml, c})" in 
    Arg.(value & opt string "" & info ["analysis"] ~doc)
  in

  let filename_flag =
    let doc = "Specify the input file" in
    Arg.(value & opt (some string) None & info ["file"] ~doc)
  in

  let engine_flag =
    let doc = "Select a single engine to use (dpll_mono, dpll_dac, sygus_dac, mixed_dac, or race (DEFAULT: dpll_mono))" in
    Arg.(value & opt engine_conv DpllMono & info ["engine"] ~docv:"ENGINE" ~doc)
  in

  let output_format_flag =
    let doc = "Select an output format (s-expression, hex, or hex-packed (DEFAULT: s-expression))" in
    Arg.(value & opt output_conv SExpression & info ["output-format"] ~docv:"ENGINE" ~doc)
  in

  let num_solutions_flag =
    let doc = "Specify the approximate number of solutions to produce when --multiple-solutions is enabled (DEFAULT: infinitely many if --multiple_solutions is enabled, and otherwise 1)" in
    Arg.(value & opt int (-1) & info ["num-solutions"] ~doc)
  in

  let starting_depth_limit_flag =
    let doc = "Starting depth limit for iterative deepening search" in 
    Arg.(value & opt int 5 & info ["starting-depth-limit"] ~doc)
  in

  let restart_rate_flag =
    let doc = "Specify the restart rate (number of iterations without finding a solution before restarting)" in 
    Arg.(value & opt int 10000 & info ["restart-rate"] ~doc)
  in

  let sols_per_iter_flag =
    let doc = "Specify the number of solutions to collect from each initial solution when --multiple_solutions is enabled (DEFAULT: 100)" in
    Arg.(value & opt int 100 & info ["sols-per-iter"] ~doc)
  in

  let seed_flag = 
    let doc = "Random seed" in 
    Arg.(value & opt (some int) None & info ["seed"] ~doc) 
  in

  let set_flags new_debug new_no_warnings new_only_parse new_show_winner 
                new_dump_clp new_multiple_solutions new_saecred new_analysis new_filename new_engine new_output_format 
                new_num_solutions new_starting_depth_limit new_restart_rate new_sols_per_iter new_seed =
    Format.pp_print_flush Format.std_formatter ();
    debug := new_debug;
    no_warnings := new_no_warnings;
    only_parse := new_only_parse;
    show_winner := new_show_winner;
    dump_clp := new_dump_clp;
    multiple_solutions := new_multiple_solutions; 
    saecred := new_saecred;
    analysis := new_analysis;
    filename := new_filename;
    selected_engine := new_engine;
    output_format := new_output_format;
    num_solutions := new_num_solutions;
    starting_depth_limit := new_starting_depth_limit; 
    restart_rate := new_restart_rate; 
    sols_per_iter := new_sols_per_iter;
    seed := new_seed;
  in

  let term =
    Term.(const set_flags
          $ debug_flag
          $ no_warnings_flag
          $ only_parse_flag
          $ show_winner_flag
          $ dump_clp_flag
          $ multiple_solutions_flag
          $ saecred_flag
          $ analysis_flag
          $ filename_flag
          $ engine_flag
          $ output_format_flag
          $ num_solutions_flag
          $ starting_depth_limit_flag 
          $ restart_rate_flag 
          $ sols_per_iter_flag
          $ seed_flag)
  in

  let info = Cmd.info "goblin" in
  let cmd = Cmd.v info term in
  let _ = Cmd.eval cmd in 
  ()
