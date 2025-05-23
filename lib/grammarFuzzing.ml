open Ast
open MutationOps
open TopologicalSort
open ByteParser
open Unix
open Lwt.Infix

type packet = bytes 
type score = float 


type packet_type = COMMIT | CONFIRM | ASSOCIATION_REQUEST | NOTHING | RESET

type grammar = ast

type mutation = Add | Delete | Modify | CrossOver | None | CorrectPacket

type mutationOperations = mutation list

type output = CRASH | TIMEOUT | EXPECTED_OUTPUT | UNEXPECTED_OUTPUT | Message of string

type provenance = RawPacket of packet | ValidPacket of packet_type

type child = (provenance list * grammar) * score

(* type single_queue = ((provenance list) * (child list)) *)

type queue_handle = NOTHING | CONFIRMED | ACCEPTED

type state_child = NOTHING of child | CONFIRMED of child | ACCEPTED of child

type population = NOTHING of child list | CONFIRMED of child list | ACCEPTED of child list

type triple_queue = population list

type trace = packet list

let sygus_success_execution_time = ref 0.0
let sygus_fail_execution_time = ref 0.0

let oracle_time = ref 0.0
let oracle_calls = ref 0

let sygus_success_calls = ref 0
let sygus_fail_calls = ref 0

let trace_time = ref 0.0

let total_execution_time = ref 0.0
let total_execution_calls = ref 0

let driver_call_time = ref 0.0
let driver_calls = ref 0

let preprocess_map (f : 'a) (p : population) : population =
  match p with
  | NOTHING x -> NOTHING (f x)
  | CONFIRMED x -> CONFIRMED (f x)
  | ACCEPTED x -> ACCEPTED (f x)

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
      (match x1, y1 with
      | COMMIT, COMMIT | CONFIRM, CONFIRM | ASSOCIATION_REQUEST, ASSOCIATION_REQUEST -> 
        compare_provenance_list xs ys
      | _, _ -> -1)
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
  | ValidPacket COMMIT -> "COMMIT"
  | ValidPacket CONFIRM -> "CONFIRM"
  | ValidPacket ASSOCIATION_REQUEST -> "ASSOCIATION_REQUEST"
  | ValidPacket RESET -> "RESET"
  | ValidPacket NOTHING -> Utils.crash "unexpected symbol.."
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

let nonterminals = ["AC_TOKEN" ; "RG_ID"; "RG_ID_LIST"; "REJECTED_GROUPS"; "RG_ID_LENGTH"; "PASSWD_ELEMENT_ID_EXTENSION"; "AC_ID_LENGTH"; "STATUS_CODE"; "RG_ELEMENT_ID_EXTENSION" ;"PASSWD_ID_LENGTH";"PASSWORD_IDENTIFIER"; "PASSWD_ELEMENT_ID" ; "RG_ELEMENT_ID"; "PASSWD_ID"; "AC_ELEMENT_ID"; "AC_TOKEN_ELEMENT"; "AC_TOKEN"; "COMMIT"; "CONFIRM"; "AUTH_SEQ_CONFIRM"; "AC_TOKEN_CONTAINER"; "CONFIRM_HASH"; "AUTH_ALGO"; "AUTH_SEQ_COMMIT"; "CONFIRM_HASH"; "SCALAR"; "GROUP_ID"; "ELEMENT"; "SEND_CONFIRM_COUNTER";]

let rec check_well_formed_rules (grammar : ast) : bool =
  match grammar with
  | [] -> true
  | ProdRule(nt, rhsList) :: xs -> (not (List.length rhsList = 1 && isNonTerminalPresent nt rhsList)) && check_well_formed_rules xs
  | TypeAnnotation(_,_,_) :: xs -> check_well_formed_rules xs
    

let rec applyMutation (m : mutation) (g : ast) (count : int) : (packet_type * grammar) option =
  Random.self_init () ;
  if count = 0 then None
  else
    let nt = random_element nonterminals in
    match m with
      Add -> print_endline "\n\nADDING\n\n" ; 
      let random_prod_rule = find_random_production_rule g in
      let added_grammar = (mutation_add_s1 g nt random_prod_rule) in
      if snd added_grammar = false then applyMutation Add g (count - 1)
      else begin
        pp_print_ast Format.std_formatter (fst added_grammar); 
        Some (NOTHING, (fst added_grammar))
      end
    | Delete -> print_endline "\n\nDELETING\n\n" ;
      let delete_attempt = (mutation_delete g nt) in
      if snd delete_attempt = false then applyMutation Delete g (count - 1)
      else
        let deleted_grammar = canonicalize (fst delete_attempt) in
        (
          match deleted_grammar with
        | Some x ->
          let well_formed_check = check_well_formed_rules x in
          (
            match well_formed_check with
            | true -> pp_print_ast Format.std_formatter x; Some (NOTHING, x)
            | false -> applyMutation Delete g (count - 1)
          )
        | None -> applyMutation Delete g (count - 1)
        )

    | Modify -> print_endline "\n\nMODIFYING\n\n" ;
      let operation = random_element [Plus; Minus] in
      let (modified_grammar, success_code) = mutation_update g nt operation in
      if success_code then 
        Some (NOTHING, modified_grammar)
      else applyMutation Modify g (count - 1)

    | CrossOver -> print_endline "\n\n\nENTERING CROSSOVER\n\n\n" ;
        let (pr1, pr2) = get_production_rules_for_crossover g in
        let nt1, nt2, po1, po2 = extract_nt_po pr1 pr2 in
        let rhs1 = random_element po1 in
        let rhs2 = random_element po2 in
        let crossoverPRs = mutation_crossover rhs1 rhs2 in
        let newPR = grammarUpdateAfterCrossover nt1 g rhs1 rhs2 crossoverPRs in
        let finalGrammar = grammarUpdateAfterCrossover nt2 newPR rhs1 rhs2 crossoverPRs in
        let canonicalizedGrammar = canonicalize finalGrammar in
          (match canonicalizedGrammar with
          | Some(x) -> 
            let well_formed_check = check_well_formed_rules x in
            (match well_formed_check with
            | true ->
              Some (NOTHING, x)
            | false -> applyMutation CrossOver g (count - 1)
            )
          | None -> (applyMutation CrossOver g (count - 1)))
    | CorrectPacket -> 
      let x = random_element [COMMIT; CONFIRM; ASSOCIATION_REQUEST] in (
        match x with 
        | COMMIT -> 
          print_endline "\n\nINJECTING CORRECT COMMIT\n\n" ;
          Some (COMMIT, g)
        | CONFIRM -> 
          print_endline "\n\nINJECTING CORRECT CONFIRM\n\n" ;
          Some (CONFIRM, g)
        | ASSOCIATION_REQUEST -> 
          print_endline "\n\nINJECTING ASSOCIATION REQUEST\n\n" ;
          Some (ASSOCIATION_REQUEST, g)
        | NOTHING -> Utils.crash "unexpected symbol NOTHING"
        | RESET -> Utils.crash "RESET should not occur"
      )
    | None -> Some (NOTHING, g)


let rec newMutatedSet (p : child list) (m : mutationOperations) (n : int) : child list = 
  match n, p, m with
  | 0, _, _ -> []
  | _, _, [] -> []
  | _, [], _ -> []
  | _, (x::xs), (mu::ms) ->
    let mutated_grammar = applyMutation mu (fst x |> snd) 10 in
    (match mutated_grammar with
    | Some (NOTHING, z) -> ((((fst x |> fst)), z), (snd x)) :: (newMutatedSet xs ms (n - 1))
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
  output_string oc (Printf.sprintf "%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f,%d,%.8f\n" trace_length (!trace_time /. (float_of_int trace_length)) !sygus_success_calls !sygus_success_execution_time !sygus_fail_calls !sygus_fail_execution_time !oracle_calls !oracle_time !driver_calls !driver_call_time);
  close_out oc

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


let rec save_queue_info queues =
  match queues with
  | NOTHING population :: xs -> 
    save_population_info "temporal-info/NOTHING-queue-info.txt" population ;
    save_queue_info xs
  | CONFIRMED population :: xs -> 
    save_population_info "temporal-info/CONFIRMED-queue-info.txt" population ;
    save_queue_info xs
  | ACCEPTED population  :: xs -> 
    save_population_info "temporal-info/ACCEPTED-queue-info.txt" population ;
    save_queue_info xs
  | [] -> ()
    
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
      | COMMIT -> "COMMIT"
      | CONFIRM -> "CONFIRM"
      | ASSOCIATION_REQUEST -> "ASSOCIATION_REQUEST"
      | NOTHING | RESET -> Utils.crash "unexpected provenance element.."
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
    driver_call_time := ((Unix.gettimeofday ()) -. driver_start_time) ;
    driver_calls := !driver_calls + 1 ;
    (driver_result, oracle_result)
    
let run_sequence (c : child) : (provenance * output) * state =
  if c = (([],[]),0.0) then ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_)
  else begin 
    let stateTransition = fst c |> fst in
    (* print_endline "\n\n\nGRAMMAR TO SYGUS:" ;
    pp_print_ast Format.std_formatter (fst c |> snd) ; *)
    let sygus_start_time = Unix.gettimeofday () in
    let removed_dead_rules_for_sygus = dead_rule_removal (fst c |> snd) "SAE_PACKET" in
    match removed_dead_rules_for_sygus with
    | Some grammar_to_sygus ->
      sygus_success_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
      sygus_success_calls := !sygus_success_calls + 1 ;
      let packetToSend_ = Lwt_main.run (timeout_wrapper 5.0 (fun () -> (Pipeline.sygusGrammarToPacket grammar_to_sygus))) in (
        match packetToSend_ with
        | Ok (packetToSend, _metadata) ->
          print_endline "SUCCESS";
          let trace_start_time = Unix.gettimeofday () in
          let driver_output = callDriver_new (run_trace stateTransition) (RawPacket packetToSend) in
          trace_time := Unix.gettimeofday () -. trace_start_time ;
          save_time_info "temporal-info/OCaml-time-info.csv" (1 + (List.length (stateTransition))) ;
          (RawPacket packetToSend, (fst driver_output)), (snd driver_output)
        | Error _ -> 
          print_endline "ERROR";
          sygus_fail_execution_time := ((Unix.gettimeofday ()) -. sygus_start_time) ;
          sygus_fail_calls := !sygus_fail_calls + 1 ;
          ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_)
      )
    | None -> ((ValidPacket NOTHING, EXPECTED_OUTPUT), IGNORE_)
  end

let executeMutatedPopulation (mutatedPopulation : child list) (old_states : state list) : (((provenance list list) * (child list)) * (state list)) * (state list) =
  print_endline "EXECUTING MUTATED POPULATION.." ;

  let _outputList = List.map run_sequence mutatedPopulation in
  (* print_endline "List.map2 first try"; *)
  let cat_mutated_population = List.map2 (fun x y -> (x, y)) mutatedPopulation _outputList in
  (* print_endline "List.map2 first try SUCCESS"; *)
  let old_new_states = List.map2 (fun x y -> (x, y)) cat_mutated_population old_states in
  (* print_endline "List.map2 second one SUCCESS"; *)
  let removed_sygus_errors = List.filter (fun x -> (fst x |> snd |> fst |> fst) <> (ValidPacket NOTHING)) old_new_states in
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

let extract_child_from_state (p : population) : child list =
  match p with
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x

let sample_population (p : population) : population =
  let population_c = extract_child_from_state p in
  let population_proportion = (float_of_int (List.length population_c)) /. 10000.0 in
  let population_choice = 2000.0 *. population_proportion in
  let new_population_top = sample_from_percentile_range population_c 0.0 50.0 (int_of_float (population_choice /. 0.8)) in
  let new_population_random = sample_from_percentile_range population_c 0.0 100.0 (int_of_float (population_choice /. 0.2)) in
  let new_population = new_population_top @ new_population_random in
  match p with
  | NOTHING _ -> NOTHING new_population
  | CONFIRMED _ -> CONFIRMED new_population
  | ACCEPTED _ -> ACCEPTED new_population

let cleanup (q : triple_queue) : population list = [sample_population (List.nth q 0); sample_population (List.nth q 1); sample_population (List.nth q 2)]


let dump_queue_info a b c =
  let _ = Unix.system ("touch " ^ "temporal-info/sample-info.txt") in
  Unix.sleepf 0.1 ;
  print_endline "saving population info.." ;
  let string_to_save = Printf.sprintf "NOTHING SAMPLE %d, CONFIRMED SAMPLE %d, ACCEPTED SAMPLE %d\n" a b c in
  append_to_file "temporal-info/sample-info.txt" string_to_save

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

let uniform_sample_from_queue (q : triple_queue) : (child list) * (state list) =
  print_endline "SAMPLING NEW POPULATION FOR MUTATION.." ;
  let np, cnf, acc = List.nth q 0, List.nth q 1, List.nth q 2 in
   
  let np_top_sample = sample_from_percentile_range (extract_child_from_state np) 0.0 (get_percentile ((extract_child_from_state np))) 15 in
  let cnf_top_sample = sample_from_percentile_range (extract_child_from_state cnf) 0.0 (get_percentile ((extract_child_from_state cnf))) 15 in
  let acc_top_sample = sample_from_percentile_range (extract_child_from_state acc) 0.0 (get_percentile ((extract_child_from_state acc))) 15 in
  let np_bottom_sample = sample_from_percentile_range (extract_child_from_state np) 0.0 100.0 5 in
  let cnf_bottom_sample = sample_from_percentile_range (extract_child_from_state cnf) 0.0 100.0 5 in
  let acc_bottom_sample = sample_from_percentile_range (extract_child_from_state acc) 0.0 100.0 5 in
  print_endline "RETURNING NEW POPULATION FOR MUTATION.." ;
  let nothing_log_info = List.length (np_top_sample @ np_bottom_sample) in
  let confirmed_log_info = List.length (cnf_top_sample @ cnf_bottom_sample) in
  let accepted_log_info = List.length (acc_top_sample @ acc_bottom_sample) in
  dump_queue_info nothing_log_info confirmed_log_info accepted_log_info ;
  let state_concatenated = (List.map (fun _ -> NOTHING_) np_top_sample @ List.map (fun _ -> NOTHING_) np_bottom_sample @ List.map (fun _ -> CONFIRMED_) cnf_top_sample @ List.map (fun _ -> CONFIRMED_) cnf_bottom_sample @ List.map (fun _ -> ACCEPTED_) acc_top_sample @ List.map (fun _ -> ACCEPTED_) acc_bottom_sample) in
  let population_concatenated = (np_top_sample @ np_bottom_sample @ cnf_top_sample @ cnf_bottom_sample @ acc_top_sample @ acc_bottom_sample) in
  let num_states = List.length (state_concatenated) in
  let num_pop = List.length (population_concatenated) in
  let outString = Printf.sprintf "%d -- %d\n" num_states num_pop in
  print_endline outString ;
  let filter_all = List.map2 (fun x y -> (x, y)) population_concatenated state_concatenated in
  let removed_duplicates = remove_duplicates filter_all in
  let samples = List.map (fun x -> fst x) removed_duplicates in
  let states = List.map (fun x -> snd x) removed_duplicates in
  (samples, states)

let population_size_across_queues (x : population) (y : population) (z : population) =
  match x, y, z with
  | NOTHING a, CONFIRMED b, ACCEPTED c  -> List.length a + List.length b + List.length c
  | _, _, _ -> Utils.crash "Queue order not maintained"

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

let rec map_packet_to_state (cl : child list) (old_states : state list) (new_states : state list) : state_child list =
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
        | IGNORE_ -> Utils.crash "unexpected IGNORE_ pattern"
      
    (* | NOTHING -> Utils.crash "unreachable case.. nothing symbol unexpected in provenance"
    | RESET -> Utils.crash "unreachable case.. reset symbol unexpected in provenance" *)

let get_child_from_state (c : state_child) : child =
  match c with 
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x

let filter_state (qh : queue_handle) (c : state_child) : bool =
  match c with
  | NOTHING _ -> if qh = NOTHING then true else false
  | CONFIRMED _ -> if qh = CONFIRMED then true else false
  | ACCEPTED _ -> if qh = ACCEPTED then true else false

let removeDuplicates (currentList : child list) : child list = 
  let s = PopulationSet.of_list currentList in 
  PopulationSet.elements s  
  
let bucket_oracle (q : triple_queue) (clist : child list) (old_states : state list) (new_states : state list) : triple_queue =
  print_endline "NEW QUEUES GENERATING.." ;
  let np, cnf, acc = extract_child_from_state (List.nth q 0), extract_child_from_state (List.nth q 1), extract_child_from_state (List.nth q 2) in
  let newPopulation = map_packet_to_state clist old_states new_states in
  let newNothingList = removeDuplicates (np @ List.map get_child_from_state (List.filter (filter_state NOTHING) newPopulation)) in
  let newConfirmedList = removeDuplicates (cnf @ List.map get_child_from_state (List.filter (filter_state CONFIRMED) newPopulation)) in
  let newAcceptedList = removeDuplicates (acc @ List.map get_child_from_state (List.filter (filter_state ACCEPTED) newPopulation)) in
  print_endline "NEW QUEUES GENERATED -- RETURNING.." ;
  [NOTHING newNothingList; CONFIRMED newConfirmedList; ACCEPTED newAcceptedList]


let dump_single_trace (trace : provenance list) : string =
  let trace_string = List.map (fun x -> 
    match x with
    | RawPacket z -> (bytes_to_hex z)
    | ValidPacket z -> ( 
      match z with
      | COMMIT -> "COMMIT"
      | CONFIRM -> "CONFIRM"
      | ASSOCIATION_REQUEST -> "ASSOCIATION_REQUEST"
      | NOTHING | RESET -> Utils.crash "unexpected provenance element.."
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

let normalize_scores (q : triple_queue) : triple_queue =
  let np, cnf, acc = List.nth q 0, List.nth q 1, List.nth q 2 in
  match np, cnf, acc with
  | NOTHING x, CONFIRMED y, ACCEPTED z -> [
    NOTHING (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) x);
    CONFIRMED (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) y);
    ACCEPTED (List.map (fun i -> (fst i), (snd i /. (float_of_int (List.length (fst i |> fst))))) z)
    ]
    | _, _, _ -> Utils.crash "unexpected queue handle"

let merge_queues (q1 : triple_queue) (q2 : triple_queue) : triple_queue = [
    NOTHING ((extract_child_from_state (List.nth q1 0)) @ (extract_child_from_state (List.nth q2 0))); 
    CONFIRMED ((extract_child_from_state (List.nth q1 1)) @ (extract_child_from_state (List.nth q2 1))); 
    ACCEPTED ((extract_child_from_state (List.nth q1 2)) @ (extract_child_from_state (List.nth q2 2)))
  ]

let save_updated_queue_sizes a b =
  let _ = Unix.system ("touch " ^ "temporal-info/queue-size-updates.txt") in
  Unix.sleepf 0.1 ;
  print_endline "write_queue_sizes" ;
  let oc = open_out "temporal-info/queue-size-updates.txt" in
  output_string oc (Printf.sprintf "Old queue size: %d -- New queue size: %d\n" a b);
  close_out oc

let save_iteration_time (iteration : int) (iteration_timer : float) : unit =
  let time_string = Printf.sprintf "Iteration: %d --> Time taken: %.3f\n" iteration iteration_timer in
  append_to_file "temporal-info/iteration-times.txt" time_string ;
  ()

let rec fuzzingAlgorithm 
(maxCurrentPopulation : int) 
(currentQueue : triple_queue) 
(iTraces : provenance list list)
(tlenBound : int) 
(currentIteration : int) 
(terminationIteration:int) 
(cleanupIteration : int) 
(newChildThreshold : int) 
(mutationOperations : mutationOperations)
(seed : triple_queue) =
  let nothing_population = List.nth currentQueue 0 in
  let confirmed_population = List.nth currentQueue 1 in
  let accepted_population = List.nth currentQueue 2 in
  let total_population_size = population_size_across_queues nothing_population confirmed_population accepted_population in
  if currentIteration >= terminationIteration then iTraces
  else
    if currentIteration mod cleanupIteration = 0 then
      fuzzingAlgorithm maxCurrentPopulation seed iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed
    else if total_population_size >= maxCurrentPopulation then
      fuzzingAlgorithm maxCurrentPopulation (cleanup currentQueue) iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed
    else
      let start_time = Unix.gettimeofday () in
      let currentQueue = 
        [NOTHING (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state nothing_population));
        CONFIRMED (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state confirmed_population));
        ACCEPTED (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state accepted_population))]
      in
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
      let old_queue_size = population_size_across_queues (List.nth currentQueue 0) (List.nth currentQueue 1) (List.nth currentQueue 2) in
      let new_queue_size = population_size_across_queues (List.nth newQueue 0) (List.nth newQueue 1) (List.nth newQueue 2) in      
      save_updated_queue_sizes old_queue_size new_queue_size ;
      save_queue_info newQueue ;
      let iteration_timer = Unix.gettimeofday () -. start_time in
      save_iteration_time (currentIteration + 1) iteration_timer ;
      fuzzingAlgorithm maxCurrentPopulation newQueue (List.append iTraces iT) tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations seed

let initialize_files () =
  initialize_clear_file "temporal-info/NOTHING-queue-info.txt";
  initialize_clear_file "temporal-info/CONFIRMED-queue-info.txt";
  initialize_clear_file "temporal-info/ACCEPTED-queue-info.txt";
  initialize_clear_file "temporal-info/queue-size-updates.txt";
  initialize_clear_file "temporal-info/OCaml-time-info.csv";
  initialize_clear_file "temporal-info/sample-info.txt";
  initialize_clear_file "results/interesting_traces.txt";
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
  let nothing_queue = NOTHING([([], commit_grammar), 0.0; ([], confirm_grammar), 0.0; ([], commit_confirm_grammar), 0.0;]) in
  let confirmed_queue = CONFIRMED([([ValidPacket COMMIT], commit_grammar), 0.0; ([ValidPacket COMMIT], confirm_grammar), 0.0; ([ValidPacket COMMIT], commit_confirm_grammar), 0.0;]) in
  let accepted_queue = ACCEPTED([([ValidPacket COMMIT; ValidPacket CONFIRM], commit_grammar), 0.0; ([ValidPacket COMMIT; ValidPacket CONFIRM], confirm_grammar), 0.0; ([ValidPacket COMMIT; ValidPacket CONFIRM], commit_confirm_grammar), 0.0;]) in

  let _ = fuzzingAlgorithm 10000 [nothing_queue; confirmed_queue; accepted_queue] [] 100 0 1150 100 100 [CorrectPacket; Modify; Add;CrossOver;Delete;] [nothing_queue; confirmed_queue; accepted_queue] in
  ()
  (* CorrectPacket;  Modify; Add;CrossOver;*)