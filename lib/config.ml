(* config.ml — Protocol-parametric fuzzer configuration.
   
   All values are mutable refs so they can be set per-protocol
   before runFuzzer is called. The rest of grammarFuzzing.ml
   reads Config.num_packets etc. unchanged.

   Usage in main.ml:
     Config.configure "rtsp";
     Config.set_instance ~protocol:"rtsp" ~mode:"3" ~instance:0;
     GrammarFuzzing.runFuzzer grammars mode
*)

(* ================================================================
   Types
   ================================================================ *)

type packet_type = int

(* type fuzzing_mode = Mode1 | Mode2 | Mode3 *)


(* ================================================================
   Mutable configuration values
   ================================================================ *)

(* Number of valid packet types (0 .. num_packets-1).
   num_packets itself is the sentinel "no packet" value. *)
let num_packets = ref 10

(* Number of state queues *)
let num_queues = ref 5

(* Grammar start symbol *)
let start_symbol = ref "START"

(* State queue prefixes: list of (queue_name, valid_packet_prefix) *)
let queue_prefixes : (string * int list) list ref = ref []


(* ================================================================
   Instance-scoped directories for parallel runs
   
   Directory layout for a run:
     runs/<protocol>/<mode>/instance_<N>/
       sync/           ← IPC with driver
       temporal-info/  ← timing diagnostics  
       results/        ← interesting traces
       profraw-dir/    ← coverage data
   ================================================================ *)

(* Root directory for this instance — all paths derived from this *)
let run_dir = ref "."

(* Derived paths *)
let sync_dir = ref "sync"
let temporal_dir = ref "temporal-info"
let results_dir = ref "results"
let profraw_dir = ref "profraw-dir"
let grammar_hex_log = ref "grammar_hex_log.txt"

(* Helper: join path components *)
let ( // ) a b = a ^ "/" ^ b

(* Build a path relative to sync_dir *)
let sync_path filename = !sync_dir // filename

(* Build a path relative to temporal_dir *)  
let temporal_path filename = !temporal_dir // filename

(* Build a path relative to results_dir *)
let results_path filename = !results_dir // filename


(* ================================================================
   Per-protocol configurations
   ================================================================ *)

let configure_ftp () =
  num_packets := 10;
  num_queues := 5;
  start_symbol := "START";
  queue_prefixes := [
    ("connected",     []);
    ("user_sent",     [0]);
    ("authenticated", [0; 1]);
    ("pasv_ready",    [0; 1; 5]);
    ("type_pasv",     [0; 1; 4; 5]);
  ]

let configure_rtsp () =
  num_packets := 10;
  num_queues := 7;
  start_symbol := "START";
  queue_prefixes := [
    ("connected",     []);
    ("options_done",  [0]);
    ("described",     [0; 1]);
    ("ready",         [0; 1; 2]);
    ("playing",       [0; 1; 2; 3]);
    ("no_options",    [1; 2]);
    ("direct_setup",  [2]);
  ]

let configure_wpa () =
  num_packets := 5;
  num_queues := 5;
  start_symbol := "START";
  queue_prefixes := [
    ("nothing",    []);
    ("confirmed",  [0]);
    ("accepted",   [0; 1]);
    ("eapol_1",    [0; 1; 2]);
    ("eapol_2",    [0; 1; 2; 3]);
  ]


(* ================================================================
   Top-level configure dispatch
   ================================================================ *)

let configure (protocol : string) =
  match protocol with
  | "ftp"  -> configure_ftp ()
  | "rtsp" -> configure_rtsp ()
  | "wpa"  -> configure_wpa ()
  | s      -> failwith (Printf.sprintf "Unknown protocol: %s (supported: ftp, rtsp, wpa)" s)


(* ================================================================
   Instance setup — call after configure, before runFuzzer.
   
   Sets run_dir and derived paths, creates directories.
   
   Config.set_instance ~protocol:"rtsp" ~mode:"3" ~instance:0
   
   Creates:
     runs/rtsp/mode3/instance_0/sync/
     runs/rtsp/mode3/instance_0/temporal-info/
     runs/rtsp/mode3/instance_0/results/
     runs/rtsp/mode3/instance_0/profraw-dir/
   ================================================================ *)

let set_instance ~protocol ~mode ~instance =
  let root = Printf.sprintf "runs/%s/mode%s/instance_%d" protocol mode instance in
  run_dir := root;
  sync_dir := root // "sync";
  temporal_dir := root // "temporal-info";
  results_dir := root // "results";
  profraw_dir := root // "profraw-dir";
  grammar_hex_log := root // "grammar_hex_log.txt";
  (* Create all directories *)
  List.iter (fun d ->
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" d) in ()
  ) [!sync_dir; !temporal_dir; !results_dir; !profraw_dir]

(* Compute a unique port for this instance to avoid collisions.
   Base port per protocol, offset by instance number.
   Usage: driver --port $(Config.instance_port) *)
let base_port = ref 2121

let configure_base_port (protocol : string) =
  match protocol with
  | "ftp"  -> base_port := 2121
  | "rtsp" -> base_port := 8554
  | "wpa"  -> base_port := 0     (* WPA doesn't use TCP *)
  | _      -> base_port := 9000

let instance_port (instance : int) =
  !base_port + instance

(* Print full config for logging / launch scripts *)
let print_instance_info (instance : int) =
  Printf.printf "=== Fuzzer Instance ===\n";
  Printf.printf "  run_dir:      %s\n" !run_dir;
  Printf.printf "  sync_dir:     %s\n" !sync_dir;
  Printf.printf "  temporal_dir: %s\n" !temporal_dir;
  Printf.printf "  results_dir:  %s\n" !results_dir;
  Printf.printf "  profraw_dir:  %s\n" !profraw_dir;
  Printf.printf "  port:         %d\n" (instance_port instance);
  Printf.printf "  num_packets:  %d\n" !num_packets;
  Printf.printf "  num_queues:   %d\n" !num_queues;
  Printf.printf "=======================\n%!"


(* ================================================================
   Helper: build initial queues from queue_prefixes × grammars
   ================================================================ *)

let build_initial_queues (grammars : 'a list) : ((int list * 'a) * float) list list =
  List.map (fun (_name, prefix) ->
    List.map (fun g -> ((prefix, g), 0.0)) grammars
  ) !queue_prefixes