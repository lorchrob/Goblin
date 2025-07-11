open Ast
open MutationOps
open TopologicalSort
open ByteParser
open Unix
open Lwt.Infix


type packet = bytes
type score = float

type grammar = ast

type mutation = Add | Delete | Modify | CrossOver | None | CorrectPacket

type mutationOperations = mutation list

type output = CRASH | TIMEOUT | EXPECTED_OUTPUT | UNEXPECTED_OUTPUT | Message of string

type provenance = RawPacket of packet | ValidPacket of packet_type

type child = (provenance list * grammar) * score

(* type single_queue = ((provenance list) * (child list)) *)

type population = child list

type queue = population list

type trace = packet list

let sygus_success_execution_time = ref 0.0
let sygus_fail_execution_time = ref 0.0

let other_success_execution_time = ref 0.0
let other_fail_execution_time = ref 0.0

let preprocessing_time = ref 0.0

let oracle_time = ref 0.0
let oracle_calls = ref 0

let sygus_success_calls = ref 0
let sygus_fail_calls = ref 0

let other_success_calls = ref 0
let other_fail_calls = ref 0

let trace_time = ref 0.0

let total_execution_time = ref 0.0
let total_execution_calls = ref 0

let driver_call_time = ref 0.0
let driver_calls = ref 0

let addition_count = ref 0
let deletion_count = ref 0
let modify_count = ref 0
let crossover_count = ref 0
let grammar_byte_map = ref ""

let rec compare_provenance_list (x : provenance list) (y : provenance list) =
  match x, y with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | xx :: xs, yy :: ys -> 
    match xx, yy with
    | RawPacket x1, RawPacket y1 -> 
      let c = Bytes.compare x1 y1 in
      if c = 0 then compare_provenance_list xs ys else c
    | ValidPacket x1, ValidPacket y1 -> 
      if x1 = y1 
        then compare_provenance_list xs ys
      else 1
    | _, _ -> -1



let compare_grammar g1 g2 =
  let s1 = Utils.capture_output pp_print_ast g1 in
  let s2 = Utils.capture_output pp_print_ast g2 in
  Stdlib.compare s1 s2
  
module PopulationSet = Set.Make(
  struct type t = child 
  let compare ((p1, g1), _) ((p2, g2), _) = 
  match compare_provenance_list p1 p2 with 
  | 0 -> compare_grammar g1 g2 
  | c -> c 
end
) 

(* type iterationCount = int  *)

(* type childSet = child list *)

let read_grammar filename =
  let ic = open_in filename in
  try
    let length = in_channel_length ic in
    let content = really_input_string ic length in
    close_in ic ;
    content
  with e ->
    close_in_noerr ic ;
    raise e

let read_from_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;

  let ic = open_in filename in
  try
    let line = input_line ic in
    close_in ic;
    Some line
  with End_of_file ->
    close_in ic;
    None

let append_to_file filename msg =
  let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 filename in
  output_string oc msg ;
  close_out oc 

let read_binary_file filename =
  let ic = open_in_bin filename in
  try
    let file_length = in_channel_length ic in
    let buffer = Bytes.create file_length in
    really_input ic buffer 0 file_length;
    close_in ic;
    if file_length > 0 then Some buffer else None
  with End_of_file ->
    close_in ic ;
    None

let write_symbol_to_file filename msg =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1;

  (* Wait until the file is empty *)
  let rec wait_until_empty () =
    let stats = Unix.stat filename in
    if stats.st_size = 0 then ()
    else (
      Unix.sleepf 0.1;
      wait_until_empty ()
    )
  in
  wait_until_empty ();
  print_endline "write_symbol_to_file";
  let oc = open_out filename in
  output_string oc msg;
  close_out oc
    

let write_to_file filename msg =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "write_to_file" ;
  let oc = open_out_bin filename in
  output_bytes oc msg;
  close_out oc

let timeout_wrapper timeout f =
  let timeout_task = Lwt_unix.sleep timeout >|= fun () -> Error "timeout" in
  let function_task = 
    Lwt.try_bind
    (fun () -> Lwt.return (f ()))
    (function
    | Ok result -> Lwt.return (Ok result)
    | Error e -> Lwt.return (Error e)
    )
    (fun ex -> Lwt.return (Error (Printexc.to_string ex))) in
  Lwt.pick [timeout_task; function_task]

  let initialize_clear_file filename =
    let _ = Unix.system ("touch " ^ filename) in
    Unix.sleepf 0.1 ;
    print_endline "clear_file" ;
    let oc = open_out filename in
    close_out oc 
  

let clear_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "clear_file" ;
  let oc = open_out filename in
  Ok (close_out oc) 

let wait_for_python_response response_file =
  let _ = Unix.system ("touch " ^ response_file) in
  Unix.sleepf 0.1 ;
  print_endline "wait_for__python" ;
  let rec loop () =
    match read_from_file response_file with
    | Some response ->
        Printf.printf "\nREADING RESPONSE FILE:%s\n" response ;
        (* Clear the file after reading *)
        let clear_file_result = Lwt_main.run (timeout_wrapper 1.0 (fun () -> (clear_file response_file))) in (
        match clear_file_result with 
        | Ok _ | Error _ ->
          if response = "CRASH" then CRASH
          else if response = "TIMEOUT" then TIMEOUT
          else if response = "EXPECTED_OUTPUT" then EXPECTED_OUTPUT
          else if response = "UNEXPECTED_OUTPUT" then UNEXPECTED_OUTPUT
          else if response = "SUCCESSFUL_TRANSITION_SUCCESS" then EXPECTED_OUTPUT
          else Message response
        )
    | None ->
        sleepf 0.1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()

let wait_for_python_bin_response response_file =
  let _ = Unix.system ("touch " ^ response_file) in
  Unix.sleepf 0.1 ;
  let rec loop () =
    match read_binary_file response_file with
    | Some response ->
        Unix.truncate response_file 0;
        response
    | None ->
        sleepf 0.1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()

let get_message message = 
  match message with
  | Message message -> message
  | _ -> Utils.crash "not message type"

let map_provenance_to_string (p : provenance) : string =
  match p with
  | ValidPacket RESET -> "RESET"
  | ValidPacket n -> Printf.sprintf "%d" n
  | RawPacket _ -> Utils.crash "handle the raw packet case"
  
(* let callDriver x =
  let message_file = "sync/message.txt" in
  let response_file = "sync/response.txt" in
  let placeholder_replaced_file = "sync/placeholders-replace.pkt" in
  match x with 
  | ValidPacket y -> 
    write_symbol_to_file message_file (map_provenance_to_string x) ; (
    match y with
    | COMMIT -> (wait_for_python_response response_file, CONFIRMED_) 
    | CONFIRM -> (wait_for_python_response response_file, ACCEPTED_)
    | ASSOCIATION_REQUEST -> (wait_for_python_response response_file, NOTHING_)
    | RESET -> (wait_for_python_response response_file, NOTHING_)
    | NOTHING -> Utils.crash "unexpected symbol.."
  )
  | RawPacket y ->
    write_to_file message_file y;
    print_endline "write to file successful.." ;
    let bin_placeholders = wait_for_python_bin_response placeholder_replaced_file in
    print_endline "possible fail place.." ;
    let string_to_send = ((Bitstring.bitstring_of_string (Bytes.to_string bin_placeholders))) in
    print_endline "string_to_send success.." ;
    print_endline (Bytes.to_string bin_placeholders) ;
    let oracle_start_time = Unix.gettimeofday () in
    let oracle_result = parse_packet string_to_send in
    oracle_time := ((Unix.gettimeofday ()) -. oracle_start_time) ;
    print_endline "oracle result success" ;
    let driver_start_time = Unix.gettimeofday () in
    let driver_result = wait_for_python_response response_file in
    driver_call_time := ((Unix.gettimeofday ()) -. driver_start_time) ;
    driver_calls := !driver_calls + 1 ;
    (driver_result, oracle_result) *)

let rec scoreFunction (pktStatus : (provenance * output) list) (mutatedPopulation : child list) : ((provenance list list) * (child list)) =
  print_endline "SCORING FUNCTION -- SCORING POPULATION" ;
  match pktStatus, mutatedPopulation with
  | [], [] | _, [] -> [], []
  | [], p -> [], p
  | (packet, output_symbol) :: xs, ((old_provenance, current_grammar), score) :: ys ->
    let future_provenance_and_population = scoreFunction xs ys in
    match output_symbol with
    | TIMEOUT | EXPECTED_OUTPUT ->
      if output_symbol = TIMEOUT then
        ((fst future_provenance_and_population)), (((old_provenance @ [packet], current_grammar), score +. 0.1) :: (snd future_provenance_and_population))
      else 
        ((fst future_provenance_and_population)), (((old_provenance @ [packet], current_grammar), score +. 0.3) :: (snd future_provenance_and_population))
    | CRASH | UNEXPECTED_OUTPUT ->
      if output_symbol = CRASH then 
        ((old_provenance @ [packet])  :: (fst future_provenance_and_population)), (((old_provenance @ [packet], current_grammar), score +. 0.5) :: (snd future_provenance_and_population))
      else 
        ((old_provenance @ [packet]) :: (fst future_provenance_and_population)), (((old_provenance @ [packet], current_grammar), score +. 0.7) :: (snd future_provenance_and_population))        
    | Message _ -> Utils.crash "Message type should not be matched in score func.."

    
(* Function to get a random element from a list *)
let random_element (lst: 'a list) : 'a =
  if lst = [] then Utils.crash "Empty list"
  else begin
    let len = List.length lst in
    let random_index = Random.int len in
    List.nth lst random_index
  end

(* Function to sample from a given percentile range *)
let sample_from_percentile_range (pop : child list) (lower_percentile: float) (upper_percentile: float) (sample_size: int) : child list =
  Random.self_init () ;
  if sample_size = 0 then pop
  else
    let sorted_pop = List.sort (fun ((_, _), score1) ((_, _), score2) -> compare score2 score1) pop in
    let pop_len = List.length sorted_pop in
    let start_index = int_of_float (lower_percentile *. float_of_int pop_len /. 100.0) in
    let end_index = int_of_float (upper_percentile *. float_of_int pop_len /. 100.0) in

    (* Helper function to slice a list *)
    let rec slice start count lst =
      match lst with
      | [] -> []
      | hd :: tl ->
          if start > 0 then slice (start - 1) count tl
          else if count > 0 then hd :: slice 0 (count - 1) tl
          else []
    in

    (* Slice the list to get the segment *)
    let segment =
      let segment_start = max start_index 0 in
      let segment_length = min (end_index - start_index + 1) (pop_len - segment_start) in
      slice segment_start segment_length sorted_pop
    in

    (* Randomly sample from the segment *)
    let rec sample acc remaining size =
      if size <= 0 || remaining = [] then acc
      else
        let elem = random_element remaining in
        sample (elem :: acc) (List.filter ((<>) elem) remaining) (size - 1)
    in

    sample [] segment sample_size


let rec check_well_formed_rules (grammar : ast) : bool =
  match grammar with
  | [] -> true
  | ProdRule(nt, rhsList, _) :: xs -> (not (List.length rhsList = 1 && isNonTerminalPresent nt rhsList)) && check_well_formed_rules xs
  | TypeAnnotation(_, _, _, _) :: xs -> check_well_formed_rules xs
    

let rec applyMutation (m : mutation) (g : ast) (count : int) : (packet_type * grammar) option =
  Random.self_init () ;
  if count = 0 then None
  else
    let nt = random_element nonterminals in
    match m with
    | Add -> print_endline "\n\nADDING\n\n" ; 
      let random_prod_rule = find_random_production_rule g in
      let added_grammar, success = (mutation_add_s1 g nt random_prod_rule) in
      if success = false then applyMutation Add g (count - 1)
      else begin
        pp_print_ast Format.std_formatter (fst added_grammar); 
        Some (Config.num_packets, (fst added_grammar))
      end
    | Delete -> print_endline "\n\nDELETING\n\n" ;
      let delete_attempt, success = (mutation_delete g nt) in
      if success = false then applyMutation Delete g (count - 1)
      else begin
        pp_print_ast Format.std_formatter delete_attempt;
        deletion_count := !deletion_count + 1; 
        Some (NOTHING, delete_attempt)
          (* let pre_start = Unix.gettimeofday () in
        let deleted_grammar = canonicalize (fst delete_attempt) in
        match deleted_grammar with
        | Some x -> 
          let well_formed_check = check_well_formed_rules x in
          (
            match well_formed_check with
            | true -> pp_print_ast Format.std_formatter x; Some (Config.num_packets, x)
            | false -> applyMutation Delete g (count - 1)
          )
        | None -> applyMutation Delete g (count - 1) *)
        (* | false -> Some (NOTHING, fst delete_attempt) *)
      end
    | Modify -> print_endline "\n\nMODIFYING\n\n" ;
      let operation = random_element [Plus; Minus] in
      let (modified_grammar, success_code) = mutation_update g nt operation in
      if success_code then 
        Some (Config.num_packets, modified_grammar)
      else applyMutation Modify g (count - 1)

    | CrossOver -> print_endline "\n\n\nENTERING CROSSOVER\n\n\n" ;
        let (pr1, pr2) = get_production_rules_for_crossover g in
        let nt1, nt2, po1, po2 = extract_nt_po pr1 pr2 in
        let rhs1 = random_element po1 in
        let rhs2 = random_element po2 in
        let crossoverPRs = mutation_crossover rhs1 rhs2 in
        let newPR = grammarUpdateAfterCrossover nt1 g rhs1 rhs2 crossoverPRs in
        let finalGrammar = grammarUpdateAfterCrossover nt2 newPR rhs1 rhs2 crossoverPRs in
        crossover_count := !crossover_count + 1;
        Some (NOTHING, finalGrammar)
        (* let pre_start = Unix.gettimeofday () in *)
        (* let canonicalizedGrammar = canonicalize finalGrammar in *)
      
          (* (match canonicalizedGrammar with
          | Some(x) -> 
            let well_formed_check = check_well_formed_rules x in
            (match well_formed_check with
            | true ->
              Some (Config.num_packets, x)
            | false -> applyMutation CrossOver g (count - 1)
            )
          | None -> (applyMutation CrossOver g (count - 1))) *)
        (* | false -> Some (NOTHING, finalGrammar)) *)
    | CorrectPacket -> 
      let x = Random.int Config.num_packets in (
        print_endline "\n\nINJECTING CORRECT PACKET\n\n" ;
        Some (x, g)
      )
    | None -> Some (Config.num_packets, g)


let rec newMutatedSet (p : child list) (m : mutationOperations) (n : int) : child list = 
  match n, p, m with
  | 0, _, _ -> []
  | _, _, [] -> []
  | _, [], _ -> []
  | _, (x::xs), (mu::ms) ->
    let mutated_grammar = applyMutation mu (fst x |> snd) 10 in
    (match mutated_grammar with
    | Some (Config.num_packets, z) -> ((((fst x |> fst)), z), (snd x)) :: (newMutatedSet xs ms (n - 1))
    | Some (y, z) -> (((fst x |> fst) @ [(ValidPacket y)], z), (snd x)) :: (newMutatedSet xs ms (n - 1))
    | None -> (([], []), 0.0) :: newMutatedSet xs ms (n - 1)
    )

let rec mutationList sampleFunction (mutationOps : mutationOperations) (n : int): mutation list =
  match n with
    0 -> []
  | _ -> sampleFunction mutationOps :: mutationList sampleFunction mutationOps (n - 1)

(* let rec sendPacketsToState (p : provenance list) : unit = 
  match p with
    [] -> ()
  | x :: xs -> let _ = callDriver x in sendPacketsToState xs *)

let save_time_info filename trace_length =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "saving time info.." ;
  let oc = open_out_gen [Open_creat; Open_append] 0o666 filename in
  output_string oc (Printf.sprintf "%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f\n" trace_length (!trace_time /. (float_of_int trace_length)) !sygus_success_calls !sygus_success_execution_time !sygus_fail_calls !sygus_fail_execution_time !other_success_calls !other_success_execution_time !other_fail_calls !other_fail_execution_time !oracle_calls !oracle_time !driver_calls !driver_call_time);
  close_out oc;
  ()

let save_population_info filename population =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "saving population info.." ;
  let oc = open_out_gen [Open_creat; Open_append] 0o666 filename in
  let scores = List.map (fun x -> snd x) population in
  let provenance_lengths = List.map (fun x -> List.length (fst x |> fst)) population in
  let concatenated_lengths_scores = List.map2 (fun x y -> Printf.sprintf "Length of trace %d -> Score %.6f\n" x y) provenance_lengths scores in
  let string_to_save = List.fold_left (fun acc x -> acc ^ x) "" concatenated_lengths_scores in
  output_string oc string_to_save ;
  close_out oc

let save_mutation_count filename = 
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "saving mutation info.." ;
  let oc = open_out filename in
  output_string oc (Printf.sprintf "Addition: %d\nDeletion: %d\nModification: %d\nCrossOver: %d" !addition_count !deletion_count !modify_count !crossover_count);
  close_out oc;
  ()

let rec save_queue_info queues =
  List.map (fun x -> save_population_info (Printf.sprintf "temporal-info/%d-queue-info.txt" x)) queues
  (* | NOTHING population :: xs -> 
    save_population_info "temporal-info/NOTHING-queue-info.txt" population ;
    save_queue_info xs
  | CONFIRMED population :: xs -> 
    save_population_info "temporal-info/CONFIRMED-queue-info.txt" population ;
    save_queue_info xs
  | ACCEPTED population  :: xs -> 
    save_population_info "temporal-info/ACCEPTED-queue-info.txt" population ;
    save_queue_info xs
  | [] -> () *)
    
(* let sendPacket (c : child) : (provenance * output) * state =
  let stateTransition = fst c |> fst in
  print_endline "\n\n\nGRAMMAR TO SYGUS:" ;
  pp_print_ast Format.std_formatter (fst c |> snd) ;
  let sygus_start_time = Unix.gettimeofday () in
  let removed_dead_rules_for_sygus = dead_rule_removal (fst c |> snd) "SAE_PACKET" in
  match removed_dead_rules_for_sygus with
  | Some grammar_to_sygus ->
    sygus_success_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
    sygus_success_calls := !sygus_success_calls + 1 ;
    let packetToSend_ = Lwt_main.run (timeout_wrapper 5.0 (fun () -> (Pipeline.sygusGrammarToPacket grammar_to_sygus))) in (
      match packetToSend_ with
      | Ok (packetToSend, _metadata) ->
        sendPacketsToState stateTransition ;
        let driver_output = callDriver (RawPacket packetToSend) in
        let _ = callDriver (ValidPacket RESET) in
        save_time_info "temporal-info/OCaml-time-info.csv" (1 + (List.length (stateTransition))) ;
        (RawPacket packetToSend, (fst driver_output)), (snd driver_output)
      | Error _ -> 
        sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
        log_grammar grammar_to_sygus;
        sygus_fail_calls := !sygus_fail_calls + 1 ;
        ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_)
    )
  | None -> ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_) *)

let bytes_to_hex (b: bytes) : string =
  let hex_of_byte byte =
    Printf.sprintf "%02x" (int_of_char byte)
  in
  let len = Bytes.length b in
  let rec loop i acc =
    if i >= len then acc
    else loop (i + 1) (acc ^ hex_of_byte (Bytes.get b i))
  in
  loop 0 ""

let run_trace (trace : provenance list) : string list =
  List.map (fun x -> 
    match x with
    | RawPacket z -> (bytes_to_hex z)
    | ValidPacket z -> ( 
      match z with
      | RESET -> Utils.crash "unexpected provenance element.."
      | n -> Printf.sprintf "%d" n
    ) 
  ) trace
  

let rec write_traces_to_files p_list i =
  match p_list with
  | [] -> ()
  | x :: xs -> 
    write_symbol_to_file (Printf.sprintf "sync/message_%d.txt" i) x ;
    write_traces_to_files xs (i + 1)

let write_trace_to_file p_list = 
  let merged_trace = List.fold_left (fun acc x -> acc ^ ", " ^ x) "" p_list in
  write_symbol_to_file "sync/message.txt" merged_trace ;
  ()


let callDriver_new packets packet =
  (* let message_file = "sync/message.txt" in *)
  let response_file = "sync/response.txt" in
  (* let placeholder_replaced_file = "sync/placeholders-replace.pkt" in *)
  match packet with 
  | ValidPacket _ -> Utils.crash "unexpected sygus output ADT.."
    (* write_symbol_to_file "sync/trace-length.txt" (Printf.sprintf "%d" ((List.length packets) + 1)) ; 
    write_traces_to_files (packets @ [(map_provenance_to_string packet)]) 0 ;
    (* write_symbol_to_file message_file (packets ^ (map_provenance_to_string packet) ^ ", ") ; ( *)
    (match y with
    | COMMIT -> (wait_for_python_response response_file, CONFIRMED_) 
    | CONFIRM -> (wait_for_python_response response_file, ACCEPTED_)
    | ASSOCIATION_REQUEST -> (wait_for_python_response response_file, NOTHING_)
    | RESET -> (wait_for_python_response response_file, NOTHING_)
    | NOTHING -> Utils.crash "unexpected symbol.."
  ) *)
  | RawPacket y ->
    (* write_symbol_to_file "sync/trace-length.txt" (Printf.sprintf "%d" ((List.length packets) + 1)) ;  *)
    let bytes_to_send = bytes_to_hex y in
    print_endline bytes_to_send;
    write_trace_to_file (packets @ [bytes_to_send]);
    parse_packet (Bitstring.bitstring_of_string (Bytes.to_string y)) ;
    (* write_symbol_to_file message_file (packets ^ (Bytes.to_string y) ^ ", "); *)
    print_endline "write to file successful.." ;
    (* let bin_placeholders = wait_for_python_bin_response placeholder_replaced_file in *)
    (* print_endline "possible fail place.." ; *)
    (* let string_to_send = ((Bitstring.bitstring_of_string (Bytes.to_string bin_placeholders))) in *)
    (* print_endline "string_to_send success.." ; *)
    (* print_endline (Bytes.to_string bin_placeholders) ; *)
    (* let oracle_start_time = Unix.gettimeofday () in *)
    let oracle_result = wait_for_oracle_response "sync/oracle-response.txt" in
    (* oracle_time := ((Unix.gettimeofday ()) -. oracle_start_time) ; *)
    (* print_endline "oracle result success" ; *)
    let driver_start_time = Unix.gettimeofday () in
    let driver_result = wait_for_python_response response_file in
    let driver_score = wait_for_score "sync/score.txt" in
    driver_call_time := ((Unix.gettimeofday ()) -. driver_start_time) ;
    driver_calls := !driver_calls + 1 ;
    (driver_result, oracle_result, driver_score)
    
let run_sequence (c : child) : (provenance * output) * state * (bool * score) =
  if c = (([],[]),0.0) then ((ValidPacket Config.num_packets, EXPECTED_OUTPUT), -1, (false, -1.0)) 
  else begin 
    let stateTransition = fst c |> fst in
    (* print_endline "\n\n\nGRAMMAR TO SYGUS:" ;
    pp_print_ast Format.std_formatter (fst c |> snd) ; *)
    let sygus_start_time = Unix.gettimeofday () in
    let removed_dead_rules_for_sygus = dead_rule_removal (fst c |> snd) Config.start_symbol in
    match removed_dead_rules_for_sygus with
    | Some grammar_to_sygus ->
      sygus_success_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
      sygus_success_calls := !sygus_success_calls + 1 ;
      (* let packetToSend_ = Lwt_main.run (timeout_wrapper 5.0 (fun () -> (Pipeline.sygusGrammarToPacket grammar_to_sygus))) in ( *)
    let timestamp = string_of_float (Unix.gettimeofday ()) in
    let packetToSend_1 = (
      let other_start_time = Unix.gettimeofday () in
      try 
        let grammar = fst c |> snd in
        Format.printf "%a\n" Ast.pp_print_ast grammar;
        let grammar_str = Format.asprintf "%a\n" Ast.pp_print_ast grammar in
        let timestamp = Unix.gettimeofday () |> string_of_float in
        let log_entry = "=== GRAMMAR [" ^ timestamp ^ "] ===\n" ^ grammar_str ^ "\n" in
        let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
        output_string oc log_entry;
        close_out oc;
        Format.pp_print_flush Format.std_formatter ();
        let sygus_ast, _, _ = Pipeline.main_pipeline ~grammar "dummy" in 
        other_success_execution_time := ((Unix.gettimeofday ()) -. other_start_time) ;
        other_success_calls := !other_success_calls + 1 ;
        let sygus_ast = BitFlips.flip_bits sygus_ast in
        (* grammar_byte_map := !grammar_byte_map ^ (Utils.capture_output Ast.pp_print_ast grammar); *)
        let byte_serial, metadata = SygusAst.serialize_bytes SygusAst.Big sygus_ast in
        (* Print hex to stdout and append to file *)
        Format.printf "%s\n" (bytes_to_hex byte_serial);
        let hex_str = "=== HEX RESULT [" ^ timestamp ^ "] ===\n" ^ (bytes_to_hex byte_serial) ^ "\n\n" in
        let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
        output_string oc hex_str;
        close_out oc;
        Format.pp_print_flush Format.std_formatter ();
        Ok (byte_serial, metadata)
      with exn -> 
        other_fail_calls := !other_fail_calls + 1;
        other_fail_execution_time := ((Unix.gettimeofday ()) -. other_start_time);
        let error = Format.asprintf "Exception: %s\n" (Printexc.to_string exn) in 
        let error2 = Format.asprintf "Backtrace:\n%s\n" (Printexc.get_backtrace ()) in 
        let error_str = "=== ERROR [" ^ timestamp ^ "] ===\n" ^ error ^ error2 ^ "\n\n" in
        let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
        output_string oc error_str;
        close_out oc;
        Format.pp_print_flush Format.std_formatter ();
        Error (error ^ error2))
      in 
      let packetToSend_2 = (
        let sygus_start_time = Unix.gettimeofday () in
        let canon_grammar = canonicalize (fst c |> snd) in
        match canon_grammar with
        | Some x -> 
          let removed_dead_rules_for_sygus = dead_rule_removal x "SAE_PACKET" in
          (match removed_dead_rules_for_sygus with
          | Some grammar_to_sygus -> (
            (* preprocessing_time := Unix.gettimeofday () -. pre_start; *)
            (* Print grammar to stdout and append to file *)
            
            let grammar = grammar_to_sygus in 
        try
          let sygus_out = Pipeline.sygusGrammarToPacket grammar in
          match sygus_out with
          | Ok (packetToSend, _metadata) ->
            sygus_success_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
            sygus_success_calls := !sygus_success_calls + 1 ;
            Format.printf "SYGUS CALL SUCCESS\n";
            (* Format.printf "PRINTING SYGUS AST:\n"; *)
            (* Format.printf "%a\n" Ast.pp_print_ast grammar; *)
            Format.printf "PRINTING byte serialization SYGUS:\n";
            (* Print hex to stdout and append to file *)
            Format.printf "%s\n" (bytes_to_hex packetToSend);
            let hex_str = "=== SYGUS HEX RESULT [" ^ timestamp ^ "] ===\n" ^ (bytes_to_hex packetToSend) ^ "\n\n" in
            let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
            output_string oc hex_str;
            close_out oc;
            Format.printf "\n";
            Format.pp_print_flush Format.std_formatter ();
            (* let trace_start_time = Unix.gettimeofday () in
            let driver_output = callDriver_new (run_trace stateTransition) (RawPacket packetToSend) in
            trace_time := Unix.gettimeofday () -. trace_start_time ;
            save_time_info "temporal-info/OCaml-time-info.csv" (1 + (List.length (stateTransition))) ; *)
            Ok (packetToSend,  _metadata)
          | Error e -> 
            sygus_fail_calls := !sygus_fail_calls + 1;
            sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time);
            let error_str = "=== ERROR [" ^ timestamp ^ "] ===\n" ^ e ^ "\n\n" in
            let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
            output_string oc error_str;
            close_out oc;
            Format.pp_print_flush Format.std_formatter ();
            (* print_endline "ERROR";
            sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
            sygus_fail_calls := !sygus_fail_calls + 1 ; *)
            (* Ok ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_) *)
            Error e
        with exn -> 
          sygus_fail_calls := !sygus_fail_calls + 1;
          sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time);
          let error = Format.asprintf "Exception: %s\n" (Printexc.to_string exn) in 
          let error2 = Format.asprintf "Backtrace:\n%s\n" (Printexc.get_backtrace ()) in 
          let error_str = "=== ERROR [" ^ timestamp ^ "] ===\n" ^ error ^ error2 ^ "\n\n" in
          log_error error_str timestamp;
          Error (error ^ error2))
        | None ->
          log_error "SyGuS: Could not remove dead rules" timestamp;
          Error "SyGuS: Could not remove dead rules")
      | None ->
        log_error "SyGuS: Could not canonicalize grammar" timestamp;
        Error ("SyGuS: Could not canonicalize grammar"))
        in
        let packetToSend_ = (fun x -> match x with | true -> packetToSend_1 | false -> packetToSend_2) flag in
        match packetToSend_ with
        | Ok (packetToSend, _metadata) ->
          (* grammar_byte_map := !grammar_byte_map ^ "\n HEX: " ^ bytes_to_hex packetToSend ^ "\n------------------------------\n"; *)
          Format.printf "\n";
          Format.pp_print_flush Format.std_formatter ();
          let trace_start_time = Unix.gettimeofday () in
          let (driver_result, oracle_result, driver_score) = callDriver_new (run_trace stateTransition) (RawPacket packetToSend) in
          trace_time := Unix.gettimeofday () -. trace_start_time ;
          save_time_info "temporal-info/OCaml-time-info.csv" (1 + (List.length (stateTransition))) ;
          ((RawPacket packetToSend, (driver_result)), oracle_result, driver_score)
        | Error e -> 
          Format.printf "ERROR\n";
          Format.printf "%s\n" e ;
          (* Log error to grammar_hex_log.txt with timestamp link *)
          (* let error_str = "=== ERROR [" ^ timestamp ^ "] ===\n" ^ e ^ "\n\n" in
          let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "grammar_hex_log.txt" in
          output_string oc error_str;
          close_out oc;
          Format.pp_print_flush Format.std_formatter (); *)
          (* sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ; *)
          (* sygus_fail_calls := !sygus_fail_calls + 1 ; *)
          ((ValidPacket Config.num_packets, EXPECTED_OUTPUT), -1, (false, -1.0))
    (* | None -> ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_) *)
  end

let executeMutatedPopulation (mutatedPopulation : child list) (old_states : state list) (flag : bool) : (((provenance list list) * (child list)) * (state list)) * (state list) =
  print_endline "EXECUTING MUTATED POPULATION.." ;

  let _outputList = List.map (run_sequence flag) mutatedPopulation in
  (* print_endline "List.map2 first try"; *)
  let cat_mutated_population = List.map2 (fun x y -> (x, y)) mutatedPopulation _outputList in
  (* print_endline "List.map2 first try SUCCESS"; *)
  let old_new_states = List.map2 (fun x y -> (x, y)) cat_mutated_population old_states in
  (* print_endline "List.map2 second one SUCCESS"; *)
  let removed_sygus_errors = List.filter (fun x -> (fst x |> snd |> fst |> fst) <> (ValidPacket Config.num_packets)) old_new_states in
  let filtered_mutated_population = List.map (fun x -> fst x |> fst) removed_sygus_errors in
  let old_states_ = List.map (fun x -> snd x) removed_sygus_errors in
  let outputList = List.map (fun x -> fst x |> snd |> fst) removed_sygus_errors in
  let expected_states = List.map (fun x -> fst x |> snd |> snd) removed_sygus_errors in
  print_endline "EXECUTED.. SENDING FOR SCORING.." ;
    (((scoreFunction outputList filtered_mutated_population), expected_states), old_states_)

(* Filter population based on standard deviation *)
let getScores (p: child list) : score list = List.map snd p

let calcMean (s:score list) : float =
  let sum = List.fold_left (+.) 0.0 s in
  sum /. float_of_int (List.length s)

let stdDev (s:score list) : float =
  let m = calcMean s in
  let variance = List.fold_left (fun a x -> a +. (x -. m) ** 2.0) 0.0 s in
  sqrt (variance /. float_of_int (List.length s))

(* let extract_child_from_state (p : population) : child list =
  match p with
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x *)

let sample_population (p : population) : population =
  let population_proportion = (float_of_int (List.length p)) /. 10000.0 in
  let population_choice = 2000.0 *. population_proportion in
  let new_population_top = sample_from_percentile_range p 0.0 50.0 (int_of_float (population_choice /. 0.8)) in
  let new_population_random = sample_from_percentile_range p 0.0 100.0 (int_of_float (population_choice /. 0.2)) in
  new_population_top @ new_population_random
  
let rec cleanup (q : queue) : population list = 
  match q with
  | [] -> []
  | x :: xs -> sample_population x @ cleanup xs
  
  (* [sample_population (List.nth q 0); sample_population (List.nth q 1); sample_population (List.nth q 2)] *)


(* let dump_queue_info queues_sizes =
  let _ = Unix.system ("touch " ^ "temporal-info/sample-info.txt") in
  Unix.sleepf 0.1 ;
  print_endline "saving population info.." ;
  let string_to_save = Printf.sprintf "NOTHING SAMPLE %d, CONFIRMED SAMPLE %d, ACCEPTED SAMPLE %d\n" a b c in
  append_to_file "temporal-info/sample-info.txt" string_to_save *)

let rec exists elem lst =
  match lst with
  | [] -> false
  | x :: xs -> if x = elem then true else exists elem xs

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | x :: xs -> if exists x xs then remove_duplicates xs else x :: remove_duplicates xs
  

let get_percentile (p : child list) : float =
  if List.length p <= 100 then 50.0
  else if List.length p <= 1000 then 25.0
  else if List.length p <= 5000 then 10.0
  else 5.0

let replicate_indices (lists : 'a list list) : int list list =
  let rec aux acc idx = function
    | [] -> List.rev acc
    | l :: rest ->
        let repeated = List.init (List.length l) (fun _ -> idx) in
        aux (repeated :: acc) (idx + 1) rest
  in
  aux [] 0 lists

let uniform_sample_from_queue (q : queue) : (child list) * (state list) =
  print_endline "SAMPLING NEW POPULATION FOR MUTATION.." ;
  (* let np, cnf, acc = List.nth q 0, List.nth q 1, List.nth q 2 in *)
  
  (* let top_sample = List.fold_left ( @ ) [] (List.map (fun x -> sample_from_percentile_range x 0.0 (get_percentile x) 15)) in *)
  let top_sample = List.map (fun x -> sample_from_percentile_range x 0.0 (get_percentile x) 15) q in
  (* let cnf_top_sample = sample_from_percentile_range (extract_child_from_state cnf) 0.0 (get_percentile ((extract_child_from_state cnf))) 15 in
  let acc_top_sample = sample_from_percentile_range (extract_child_from_state acc) 0.0 (get_percentile ((extract_child_from_state acc))) 15 in *)
  (* let bottom_sample = List.fold_left ( @ ) [] (List.map (fun x -> sample_from_percentile_range x 0.0 100.0 5)) in *)
  let bottom_sample = List.map (fun x -> sample_from_percentile_range x 0.0 100.0 5) q in
  (* sample_from_percentile_range (extract_child_from_state np) 0.0 100.0 5 in *)
  (* let cnf_bottom_sample = sample_from_percentile_range (extract_child_from_state cnf) 0.0 100.0 5 in
  let acc_bottom_sample = sample_from_percentile_range (extract_child_from_state acc) 0.0 100.0 5 in *)
  print_endline "RETURNING NEW POPULATION FOR MUTATION.." ;
  (* let log_info = List.length (top_sample @ bottom_sample) in *)
  (* let confirmed_log_info = List.length (cnf_top_sample @ cnf_bottom_sample) in
  let accepted_log_info = List.length (acc_top_sample @ acc_bottom_sample) in *)
  (* dump_queue_info log_info ; *)
  let state_concatenated = List.fold_left ( @ ) [] (List.map2 (fun x y -> x @ y) (replicate_indices top_sample) (replicate_indices bottom_sample)) in
  let population_concatenated = (List.fold_left ( @ ) [] top_sample) @ (List.fold_left ( @ ) [] bottom_sample) in
  let num_states = List.length (state_concatenated) in
  let num_pop = List.length (population_concatenated) in
  let outString = Printf.sprintf "%d -- %d\n" num_states num_pop in
  print_endline outString ;
  let filter_all = List.map2 (fun x y -> (x, y)) population_concatenated state_concatenated in
  let removed_duplicates = remove_duplicates filter_all in
  let samples = List.map (fun x -> fst x) removed_duplicates in
  let states = List.map (fun x -> snd x) removed_duplicates in
  (samples, states)

let rec population_size_across_queues (currSize : int) (q : queue) =
  match q with
  | [] -> currSize
  | x :: xs -> population_size_across_queues (currSize + List.length x) xs

  (* let oracle (pkt : provenance) : queue_handle =
  match pkt with
  | RawPacket _ -> NOTHING
  | ValidPacket x ->
    match x with
    | COMMIT -> CONFIRMED
    | CONFIRM -> ACCEPTED 
    | ASSOCIATION_REQUEST -> ACCEPTED 
    | NOTHING -> NOTHING 
    | RESET -> NOTHING *)

let save_grammar_map filename =
  let oc = open_out filename in
  output_string oc !grammar_byte_map;
  close_out oc;
  ()

(* let rec map_packet_to_state (cl : child list) (old_states : state list) (new_states : state list) : state_child list =
  match cl, old_states, new_states with
  | [], [], [] -> []
  | [], _, _ -> Utils.crash "child list exhausted, state list non-empty"
  | _, [], _ -> Utils.crash "state list exhausted, child list non-empty"
  | _, _, [] -> Utils.crash "state list exhausted, child list non-empty"
  
  | x :: xs, y :: ys, z :: zs -> 
    match z with 
      | NOTHING_ -> NOTHING x :: map_packet_to_state xs ys zs
      | CONFIRMED_ -> CONFIRMED x :: map_packet_to_state xs ys zs
      | ACCEPTED_ -> ACCEPTED x :: map_packet_to_state xs ys zs
      | IGNORE_ -> 
        match y with
        | NOTHING_ -> NOTHING x :: map_packet_to_state xs ys zs
        | CONFIRMED_ -> CONFIRMED x :: map_packet_to_state xs ys zs
        | ACCEPTED_ -> ACCEPTED x :: map_packet_to_state xs ys zs
        | IGNORE_ -> Utils.crash "unexpected IGNORE_ pattern" *)
      
    (* | NOTHING -> Utils.crash "unreachable case.. nothing symbol unexpected in provenance"
    | RESET -> Utils.crash "unreachable case.. reset symbol unexpected in provenance" *)

(* let get_child_from_state (c : state_child) : child =
  match c with 
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x *)
(* 
let filter_state (qh : queue_handle) (c : state_child) : bool =
  match c with
  | NOTHING _ -> if qh = NOTHING then true else false
  | CONFIRMED _ -> if qh = CONFIRMED then true else false
  | ACCEPTED _ -> if qh = ACCEPTED then true else false *)

let removeDuplicates (currentList : child list) : child list = 
  let s = PopulationSet.of_list currentList in 
  PopulationSet.elements s 

let rec filter_states (i : int) (c : (state * child) list) : ((state * child) list) list = 
  if i = Config.num_queues then []
  else [(List.filter (fun x -> fst x = i) c)] @ filter_states (i + 1) c

let new_queue (c : child list) (new_states : state list) : queue =
  let combined_queue = List.map2 (fun x y -> (x, y)) new_states c in
  List.map (fun x -> List.map (fun y -> snd y) x) (filter_states 0 combined_queue)

let bucket_oracle (q : queue) (clist : child list) (_old_states : state list) (new_states : state list) : queue =
  print_endline "NEW QUEUES GENERATING.." ;
  List.map2 (fun x y -> x @ y) q (new_queue clist new_states)
  (* let np, cnf, acc = extract_child_from_state (List.nth q 0), extract_child_from_state (List.nth q 1), extract_child_from_state (List.nth q 2) in *)
  (* let newPopulation = map_packet_to_state clist old_states new_states in
  let newNothingList = removeDuplicates (np @ List.map get_child_from_state (List.filter (filter_state NOTHING) newPopulation)) in
  let newConfirmedList = removeDuplicates (cnf @ List.map get_child_from_state (List.filter (filter_state CONFIRMED) newPopulation)) in
  let newAcceptedList = removeDuplicates (acc @ List.map get_child_from_state (List.filter (filter_state ACCEPTED) newPopulation)) in *)
  (* print_endline "NEW QUEUES GENERATED -- RETURNING.." ; *)
  (* [NOTHING newNothingList; CONFIRMED newConfirmedList; ACCEPTED newAcceptedList] *)

let dump_single_trace (trace : provenance list) : string =
  let trace_string = List.map (fun x -> 
    match x with
    | RawPacket z -> (bytes_to_hex z)
    | ValidPacket z -> ( 
      (* match z with *)
      Printf.sprintf "%d" z
      (* | RESET -> Utils.crash "unexpected provenance element.." *)
    )
  ) trace in
  let result = ref "" in
  (List.iter (fun x -> result := !result ^ x ^ ", ") trace_string) ;
  !result ^ "\n"  

let dump_all_traces (traces : provenance list list) =
  let trace_string_lists = List.map dump_single_trace traces in
  let result = ref "" in
  (List.iter (fun x -> result := !result ^ x ) trace_string_lists) ;
  append_to_file "results/interesting_traces.txt" !result

let normalize_scores (q : queue) : queue =
  List.map (fun j -> List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) j) q
  (* let np, cnf, acc = List.nth q 0, List.nth q 1, List.nth q 2 in
  match np, cnf, acc with
  | NOTHING x, CONFIRMED y, ACCEPTED z -> [
    NOTHING (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) x);
    CONFIRMED (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) y);
    ACCEPTED (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) z)
    ]
    | _, _, _ -> Utils.crash "unexpected queue handle" *)


(* let merge_queues (q1 : triple_queue) (q2 : triple_queue) : triple_queue = [
    NOTHING ((extract_child_from_state (List.nth q1 0)) @ (extract_child_from_state (List.nth q2 0))); 
    CONFIRMED ((extract_child_from_state (List.nth q1 1)) @ (extract_child_from_state (List.nth q2 1))); 
    ACCEPTED ((extract_child_from_state (List.nth q1 2)) @ (extract_child_from_state (List.nth q2 2)))
  ] *)

let save_updated_queue_sizes a b =
  let _ = Unix.system ("touch " ^ "temporal-info/queue-size-updates.txt") in
  Unix.sleepf 0.1 ;
  print_endline "write_queue_sizes" ;
  let oc = open_out "temporal-info/queue-size-updates.txt" in
  output_string oc (Printf.sprintf "Old queue size: %d -- New queue size: %d\n" a b);
  close_out oc;
  ()

let save_iteration_time (iteration : int) (iteration_timer : float) : unit =
  let time_string = Printf.sprintf "Iteration: %d --> Time taken: %.3f\n" iteration iteration_timer in
  append_to_file "temporal-info/iteration-times.txt" time_string ;
  ()


let rec fuzzingAlgorithm 
(maxCurrentPopulation : int) 
(currentQueue : queue) 
(iTraces : provenance list list)
(tlenBound : int) 
(currentIteration : int) 
(terminationIteration:int) 
(cleanupIteration : int) 
(newChildThreshold : int) 
(mutationOperations : mutationOperations)
(seed : queue)
(flag : bool) =
  let total_population_size = population_size_across_queues 0 currentQueue in
  if currentIteration >= terminationIteration then iTraces
  else
    if currentIteration mod cleanupIteration = 0 then
      fuzzingAlgorithm maxCurrentPopulation seed iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed flag
    else if total_population_size >= maxCurrentPopulation then
      fuzzingAlgorithm maxCurrentPopulation (cleanup currentQueue) iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed flag 
    else
      let start_time = Unix.gettimeofday () in
      let currentQueue = List.map (fun y -> List.filter (fun x -> List.length (fst x |> fst) <= 10) y) currentQueue in
      let sampled_pop = uniform_sample_from_queue currentQueue in
      let newPopulation = fst sampled_pop in
      let old_states_ = snd sampled_pop in
      let selectedMutations = mutationList random_element mutationOperations (List.length newPopulation) in
      let mutatedPopulation = newMutatedSet newPopulation selectedMutations (List.length newPopulation) in
      let score_and_oracle_old_states = executeMutatedPopulation mutatedPopulation old_states_ in
      let old_states = snd score_and_oracle_old_states in
      let score_and_oracle = fst score_and_oracle_old_states in
      let (iT, newPopulation_) = fst score_and_oracle in
      dump_all_traces iT ;
      let newQueue = normalize_scores (bucket_oracle currentQueue newPopulation_ old_states (snd score_and_oracle)) in
      let old_queue_size = population_size_across_queues 0 currentQueue in
      let new_queue_size = population_size_across_queues 0 newQueue in
      save_updated_queue_sizes old_queue_size new_queue_size ;
      (* save_queue_info newQueue ; *)
      let iteration_timer = Unix.gettimeofday () -. start_time in
      save_iteration_time (currentIteration + 1) iteration_timer ;
      fuzzingAlgorithm maxCurrentPopulation newQueue (List.append iTraces iT) tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed

let initialize_files () =
  clear_state_files 0;
  initialize_clear_file "temporal-info/queue-size-updates.txt";
  initialize_clear_file "temporal-info/OCaml-time-info.csv";
  initialize_clear_file "temporal-info/sample-info.txt";
  initialize_clear_file "results/interesting_traces.txt";
  initialize_clear_file "grammar_hex_log.txt";
  (* initialize_clear_file "sync/driver_oracle.json"; *)
  initialize_clear_file "sync/oracle-response.txt";
  (* initialize_clear_file "sync/placeholders-replace.pkt"; *)
  initialize_clear_file "sync/message.txt";
  initialize_clear_file "sync/response.txt";
  ()

let runFuzzer grammar_list = 
  Random.self_init ();
  initialize_files ();
  let commit_grammar = List.nth grammar_list 0 in
  let confirm_grammar = List.nth grammar_list 1 in
  let commit_confirm_grammar = List.nth grammar_list 2 in
  let nothing_queue = [([], commit_grammar), 0.0; ([], confirm_grammar), 0.0; ([], commit_confirm_grammar), 0.0;] in
  let confirmed_queue = [([ValidPacket 0], commit_grammar), 0.0; ([ValidPacket 0], confirm_grammar), 0.0; ([ValidPacket 0], commit_confirm_grammar), 0.0;] in
  let accepted_queue = [([ValidPacket 0; ValidPacket 1], commit_grammar), 0.0; ([ValidPacket 0; ValidPacket 1], confirm_grammar), 0.0; ([ValidPacket 0; ValidPacket 1], commit_confirm_grammar), 0.0;] in
  
  (* let _ = fuzzingAlgorithm 10000 [nothing_queue;] [] 100 0 1150 100 100 [CorrectPacket;] [nothing_queue;] in
  () *)
  let _ = fuzzingAlgorithm 10000 [nothing_queue; confirmed_queue; accepted_queue] [] 100 0 1150 100 100 [CorrectPacket;] [nothing_queue; confirmed_queue; accepted_queue] in
  ()