open Ast
open Mutationops
open Topological_sort

(* DANIYAL: File with basic mutation examples

open Ast
open SygusAst

(*  <S> ::= <A> <B> <C> 
    <A> ::= <B> <C>
    <C> ::= <D>
    ---> 
    <S> ::= <B> <A> <C>
    <A> ::= <C> <B>
    <C> ::= 
*)
(* Mutating the grammar, but without a concrete packet. *)
let mutate_grammar: ast -> ast 
= fun ast -> List.map (fun element -> match element with
  | ProdRule (nt, Rhs (ge1 :: ge2 :: ges, scs) :: rules) -> ProdRule (nt, Rhs (ge2 :: ge1 :: ges, scs) :: rules)
  | ProdRule (nt, _) -> ProdRule (nt, [])
  | TypeAnnotation _ -> element
  ) ast

let has_rgid_length_with_value_2 _ = assert false 

let has_rgid_list_with_length_n _ = assert false 

let mutate _ = assert false

(* Mutating a concrete packet *)
let rec mutate_concrete_packet: sygus_ast -> sygus_ast
= fun sygus_ast -> match sygus_ast with 
| Node (constructor, children) -> 
  if constructor = "REJECTED_GROUPS" && 
     has_rgid_length_with_value_2 children && 
     has_rgid_list_with_length_n children 
  then mutate sygus_ast  
  else Node (constructor, List.map mutate_concrete_packet children)
| _ -> sygus_ast *)


(* Deletes the first grammar element from every production rule 
   of the grammar *)
(* let delete: ast -> ast 
(* Apply the mutation to every production rule in the ast *)
= fun ast -> List.map (fun element -> match element with
  (* Remove the first element of the first Rhs of each production rule *)
  | ProdRule (nt, Rhs (_ :: ges, scs) :: rules) -> 
    ProdRule (nt, Rhs (ges, scs) :: rules)
    (* If the production rule does not have an Rhs with at least one element, ignore it *)
    | ProdRule _ -> element
    (* Ignore type annotations *)
    | TypeAnnotation _ -> element
    ) ast *)
    
open Unix
    
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

let preprocess_map (f : 'a) (p : population) : population =
  match p with
  | NOTHING x -> NOTHING (f x)
  | CONFIRMED x -> CONFIRMED (f x)
  | ACCEPTED x -> ACCEPTED (f x)


(* type iterationCount = int  *)

(* type childSet = child list *)

let read_from_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "read_from_file" ;

  let ic = open_in filename in
  try
    let line = input_line ic in
    close_in ic;
    Some line
  with End_of_file ->
    close_in ic;
    None

let write_symbol_to_file filename msg =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "write_to_file" ;
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

let clear_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "clear_file" ;
  let oc = open_out filename in
  close_out oc  (* Opens and immediately closes the file to clear its content *)

let wait_for_python_response response_file =
  let _ = Unix.system ("touch " ^ response_file) in
  Unix.sleepf 0.1 ;
  print_endline "wait_for__python" ;
  let rec loop () =
    match read_from_file response_file with
    | Some response ->
        (* Clear the file after reading *)
        clear_file response_file;
        if response = "CRASH" then CRASH
        else if response = "TIMEOUT" then TIMEOUT
        else if response = "EXPECTED_OUTPUT" then EXPECTED_OUTPUT
        else if response = "UNEXPECTED_OUTPUT" then UNEXPECTED_OUTPUT
        else if response = "SUCCESSFUL_TRANSITION_SUCCESS" then EXPECTED_OUTPUT
        else Message response
    | None ->
        sleepf 0.1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()
let get_message message = 
  match message with
  | Message message -> message
  | _ -> failwith "not message type"

let map_provenance_to_string (p : provenance) : string =
  match p with
  | ValidPacket COMMIT -> "COMMIT"
  | ValidPacket CONFIRM -> "CONFIRM"
  | ValidPacket ASSOCIATION_REQUEST -> "ASSOCIATION_REQUEST"
  | ValidPacket RESET -> "RESET"
  | ValidPacket NOTHING -> failwith "unexpected symbol.."
  | RawPacket _ -> failwith "handle the raw packet case"
  
let callDriver x =
  let message_file = "/home/pirwani/Desktop/message.txt" in
  let response_file = "/home/pirwani/Desktop/response.txt" in

  match x with 
  | ValidPacket _ -> 
    write_symbol_to_file message_file (map_provenance_to_string x);
    wait_for_python_response response_file
  | RawPacket y -> 
    write_to_file message_file y;
    wait_for_python_response response_file


let rec scoreFunction (pktStatus : (provenance * output) list) (mutatedPopulation : child list) : ((provenance list list) * (child list)) =
  match pktStatus, mutatedPopulation with
  | [], [] | _, [] -> [], []
  | [], p -> [], p
  | (packet, output_symbol) :: xs, ((old_provenance, current_grammar), score) :: ys ->
    match output_symbol with
    | TIMEOUT | EXPECTED_OUTPUT ->
      if output_symbol = TIMEOUT then
        ((fst (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.3) :: (snd (scoreFunction xs ys)))
      else 
        ((fst (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.1) :: (snd (scoreFunction xs ys)))
    | CRASH | UNEXPECTED_OUTPUT ->
      if output_symbol = CRASH then 
        ((old_provenance @ [packet])  :: (fst (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.7) :: (snd (scoreFunction xs ys)))
      else 
        ((old_provenance @ [packet]) :: (fst (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.9) :: (snd (scoreFunction xs ys)))        
    | Message _ -> failwith "Message type should not be matched in score func.."

    
(* Function to get a random element from a list *)
let random_element (lst: 'a list) : 'a =
  if lst = [] then failwith "Empty list"
  else begin
    let len = List.length lst in
    let random_index = Random.int len in
    List.nth lst random_index
  end

(* Function to sample from a given percentile range *)
let sample_from_percentile_range (pop : child list) (lower_percentile: float) (upper_percentile: float) (sample_size: int) : child list =
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

let nonterminals = ["RG_ID_LIST"; "REJECTED_GROUPS"; "AC_TOKEN"; "AC_TOKEN_CONTAINER"; "SCALAR"; "GROUP_ID"; "ELEMENT";]

let rec check_well_formed_rules (grammar : ast) : bool =
  match grammar with
  | [] -> true
  | ProdRule(nt, rhsList) :: xs -> (not (List.length rhsList = 1 && isNonTerminalPresent nt rhsList)) && check_well_formed_rules xs
  | TypeAnnotation(_,_,_) :: xs -> check_well_formed_rules xs
    

let rec applyMutation (m : mutation) (g : ast) : packet_type * grammar =
  let nt = random_element nonterminals in
  match m with
    Add -> print_endline "\n\nADDING\n\n" ; 
    let added_grammar = fst (mutation_add_s1 g nt) in
    pp_print_ast Format.std_formatter added_grammar ;
    NOTHING, g
    (* NOTHING, (mutate_till_success added_grammar g Add) *)

  | Delete -> print_endline "\n\nDELETING\n\n" ;
    let deleted_grammar = dead_rule_removal (fst (mutation_delete g nt)) "SAE_PACKET" in
    (
      match deleted_grammar with
    | Some x ->
      let well_formed_check = check_well_formed_rules x in
      (
        match well_formed_check with
        | true -> NOTHING, x
          (* let sygus_pass = (mutate_till_success deleted_grammar g Delete) in
          (match sygus_pass with
          | Some x -> pp_print_ast Format.std_formatter x ; NOTHING, x
          | None -> applyMutation Delete g 
          ) *)
        | false -> applyMutation Delete g
      )
    | None -> applyMutation Delete g
    )
    (* print_endline nt ; *)
    (* pp_print_ast Format.std_formatter (dead_rule_removal (mutate_till_success deleted_grammar g Delete) "SAE_PACKET") ; *)
    (* NOTHING, dead_rule_removal (mutate_till_success deleted_grammar g Delete) "SAE_PACKET" *)

  | Modify -> print_endline "\n\nMODIFYING\n\n" ;
    let modified_grammar = fst (mutation_delete g nt) in
    pp_print_ast Format.std_formatter modified_grammar ;
    NOTHING, g
    (* NOTHING, (mutate_till_success modified_grammar g Modify) *)

  | CrossOver -> print_endline "\n\n\nENTERING CROSSOVER\n\n\n" ;
      let (pr1, pr2) = get_production_rules_for_crossover g in
      let nt1, nt2, po1, po2 = extract_nt_po pr1 pr2 in
      let rhs1 = random_element po1 in
      let rhs2 = random_element po2 in
      let crossoverPRs = mutation_crossover rhs1 rhs2 in
      let newPR = grammarUpdateAfterCrossover nt1 g rhs1 rhs2 crossoverPRs in
      let finalGrammar = grammarUpdateAfterCrossover nt2 newPR rhs1 rhs2 crossoverPRs in
      let canonicalizedGrammar = dead_rule_removal finalGrammar "SAE_PACKET" in
        (match canonicalizedGrammar with
        | Some(x) -> pp_print_ast Format.std_formatter x; 
          let well_formed_check = check_well_formed_rules x in
          (match well_formed_check with
          | true ->
                (* NOTHING,  (mutate_till_success x g CrossOver) *)
            NOTHING, x
          | false -> applyMutation CrossOver g
          )
        | None -> (applyMutation CrossOver g))
            (* pp_print_ast Format.std_formatter finalGrammar ; *)
      (* print_endline "\n\n\nEXITING CROSSOVER\n\n\n" ; *)
      (* finalGrammar *)
  | CorrectPacket -> 
    let x = random_element [COMMIT; CONFIRM; ASSOCIATION_REQUEST] in (
      match x with 
      | COMMIT -> 
        print_endline "\n\nINJECTING CORRECT COMMIT\n\n" ;
        COMMIT, g
      | CONFIRM -> 
        print_endline "\n\nINJECTING CORRECT CONFIRM\n\n" ;
        CONFIRM, g
      | ASSOCIATION_REQUEST -> 
        print_endline "\n\nINJECTING ASSOCIATION REQUEST\n\n" ;
        ASSOCIATION_REQUEST, g
      | NOTHING -> failwith "unexpected symbol NOTHING"
      | RESET -> failwith "RESET should not occur"
    )
  | None -> NOTHING, g
(* and 
  mutate_till_success (mutated_grammar : ast) (original_grammar : ast) (mutation_op : mutation) : ast = 
  let sygusOutput = (Pipeline.sygusGrammarToPacket mutated_grammar) in
    match sygusOutput with
    | Ok _ -> mutated_grammar cnf_sample 
    | Error _ -> 
      log_grammar mutated_grammar ;
      print_endline "sygus_error, retrying\n\n" ;
      let new_nt = random_element nonterminals in
      match mutation_op with
      | Add ->
        let updated_mutation = first (mutation_add_s1 original_grammar new_nt) in
        mutate_till_success updated_mutation original_grammar mutation_op
      | Delete -> 
        let updated_mutation = first (mutation_delete original_grammar new_nt) in
        let dead_rule_removed = Topological_sort.dead_rule_removal (mutate_till_success updated_mutation original_grammar mutation_op) "SAE_PACKET" in
        (match dead_rule_removed with
        | Some x -> mutate_till_success x original_grammar mutation_op
        | None -> let tupleMutation = applyMutation Delete original_grammar in
          match tupleMutation with
          (_, x) -> x
        )
      | Modify ->
        let updated_mutation = first (mutation_update original_grammar new_nt) in
        mutate_till_success updated_mutation original_grammar mutation_op
      | CrossOver ->
        let (pr1, pr2) = get_production_rules_for_crossover original_grammar in
        let nt1, nt2, po1, po2 = extract_nt_po pr1 pr2 in
        let rhs1 = random_element po1 in
        let rhs2 = random_element po2 in
        let crossoverPRs = mutation_crossover rhs1 rhs2 in
        let newPR = grammarUpdateAfterCrossover nt1 original_grammar rhs1 rhs2 crossoverPRs in
        let finalGrammar = grammarUpdateAfterCrossover nt2 newPR rhs1 rhs2 crossoverPRs in
        let canonicalizedGrammar = dead_rule_removal finalGrammar "SAE_PACKET" in
          (match canonicalizedGrammar with
          | Some(x) -> pp_print_ast Format.std_formatter x; 
            (mutate_till_success x original_grammar mutation_op)
          | None -> let tupleMutation = applyMutation CrossOver original_grammar in
            match tupleMutation with
            (_, x) -> x
          )
      | CorrectPacket -> failwith "correctpacket mutation shouldnt have been passed to this func.."
      | None -> original_grammar *)


let rec newMutatedSet (p : child list) (m : mutationOperations) (n : int) : child list = 
  match n, p, m with
  | 0, _, _ -> []
  | _, _, [] -> []
  | _, [], _ -> []
  | _, (x::xs), (mu::ms) ->
    let mutated_grammar = applyMutation mu (fst x |> snd) in
    (match mutated_grammar with
    | (NOTHING, z) -> ((((fst x |> fst)), z), 0.0) :: (newMutatedSet xs ms (n - 1))
    | (y, z) -> (((fst x |> fst) @ [(ValidPacket y)], z), 0.0) :: (newMutatedSet xs ms (n - 1))
      (* (((first x |> first), mutated_grammar), 0.0) :: (newMutatedSet xs ms (n - 1)) *)
    )
let rec mutationList sampleFunction (mutationOps : mutationOperations) (n : int): mutation list =
  match n with
    0 -> []
  | _ -> sampleFunction mutationOps :: mutationList sampleFunction mutationOps (n - 1)

let rec sendPacketsToState (p : provenance list) : unit = 
  match p with
    [] -> ()
  | x :: xs -> let _ = callDriver x in sendPacketsToState xs

let sendPacket (c : child) : (provenance * output) =
  let stateTransition = fst c |> fst in
  let packetToSend_ = (Pipeline.sygusGrammarToPacket (fst c |> snd)) in 
    match packetToSend_ with
    | Ok (packetToSend, _metadata) ->
      sendPacketsToState stateTransition ;
      let driver_output = callDriver (RawPacket packetToSend) in
      let _ = callDriver (ValidPacket RESET) in
      (RawPacket packetToSend, driver_output)
    | Error _ -> (ValidPacket NOTHING, EXPECTED_OUTPUT)
      
let executeMutatedPopulation (mutatedPopulation : child list) : ((provenance list list) * (child list)) =
  let outputList = List.map sendPacket mutatedPopulation in
    scoreFunction outputList mutatedPopulation

(* Filter population based on standard deviation *)
let getScores (p: child list) : score list = List.map snd p

let calcMean (s:score list) : float =
  let sum = List.fold_left (+.) 0.0 s in
  sum /. float_of_int (List.length s)

let stdDev (s:score list) : float =
  let m = calcMean s in
  let variance = List.fold_left (fun a x -> a +. (x -. m) ** 2.0) 0.0 s in
  sqrt (variance /. float_of_int (List.length s))


let cleaupPopulation (q : triple_queue) : triple_queue =
  (* check scores for staleness, remove population that has scores with little to no SD, 
     ignore 0.0 when checking for staleness *)
  let np = List.nth q 0 in
  let cnf = List.nth q 1 in
  let acc = List.nth q 2 in
  match np, cnf, acc with
  | NOTHING a, CONFIRMED b, ACCEPTED c ->
    let s = getScores a in
    let sd = stdDev s in
    let newNothingList = List.filter (fun (_, sc) -> sc = 0.0 || sc > sd) a in
    let s = getScores b in 
    let sd = stdDev s in
    let newConfirmedList = List.filter (fun (_, sc) -> sc = 0.0 || sc > sd) b in
    let s = getScores c in 
    let sd = stdDev s in
    let newAcceptedList = List.filter (fun (_, sc) -> sc = 0.0 || sc > sd) c in
    [NOTHING newNothingList; CONFIRMED newConfirmedList; ACCEPTED newAcceptedList]
  | _, _, _ -> failwith "Unexpected queue pattern in cleanup"

    (* END CLEANUP *)

let extract_child_from_state (p : population) : child list =
  match p with
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x

let uniform_sample_from_queue (q : triple_queue) (n : int) : child list =
  let np, cnf, acc = List.nth q 0, List.nth q 1, List.nth q 2 in
  let np_sample = sample_from_percentile_range (extract_child_from_state np) 0.0 100.0 n in
  let cnf_sample = sample_from_percentile_range (extract_child_from_state cnf) 0.0 100.0 n in
  let acc_sample = sample_from_percentile_range (extract_child_from_state acc) 0.0 100.0 n in
    np_sample @ cnf_sample @ acc_sample

let population_size_across_queues (x : population) (y : population) (z : population) =
  match x, y, z with
  | NOTHING a, CONFIRMED b, ACCEPTED c  -> List.length a + List.length b + List.length c
  | _, _, _ -> failwith "Queue order not maintained"

let oracle (pkt : provenance) : queue_handle =
  match pkt with
  | RawPacket _ -> NOTHING
  | ValidPacket x ->
    match x with
    | COMMIT -> CONFIRMED
    | CONFIRM -> ACCEPTED 
    | ASSOCIATION_REQUEST -> ACCEPTED 
    | NOTHING -> NOTHING 
    | RESET -> NOTHING

let rec map_packet_to_state (cl : child list) : state_child list =
  match cl with
  | [] -> []
  | x :: xs -> 
    let last_packet = List.hd (List.rev (fst (fst x))) in
    let state = oracle last_packet in
    match state with 
    | NOTHING -> CONFIRMED x :: map_packet_to_state xs
    | CONFIRMED -> ACCEPTED x :: map_packet_to_state xs
    | ACCEPTED -> NOTHING x :: map_packet_to_state xs
    (* | NOTHING -> failwith "unreachable case.. nothing symbol unexpected in provenance"
    | RESET -> failwith "unreachable case.. reset symbol unexpected in provenance" *)

let get_child_from_state (c : state_child) : child =
  match c with 
  | NOTHING x | CONFIRMED x | ACCEPTED x -> x

let filter_state (qh : queue_handle) (c : state_child) : bool =
  match c with
  | NOTHING _ -> if qh = NOTHING then true else false
  | CONFIRMED _ -> if qh = CONFIRMED then true else false
  | ACCEPTED _ -> if qh = ACCEPTED then true else false
  
let bucket_oracle (q : triple_queue) (clist : child list) : triple_queue =
  let np, cnf, acc = extract_child_from_state (List.nth q 0), extract_child_from_state (List.nth q 1), extract_child_from_state (List.nth q 2) in
  let newPopulation = map_packet_to_state clist in
  let newNothingList = np @ List.map get_child_from_state (List.filter (filter_state NOTHING) newPopulation) in
  let newConfirmedList = cnf @ List.map get_child_from_state (List.filter (filter_state CONFIRMED) newPopulation) in
  let newAcceptedList = acc @ List.map get_child_from_state (List.filter (filter_state ACCEPTED) newPopulation) in
   [NOTHING newNothingList; CONFIRMED newConfirmedList; ACCEPTED newAcceptedList]

let rec fuzzingAlgorithm 
(maxCurrentPopulation : int) 
(currentQueue : triple_queue) 
(iTraces : provenance list list)
(tlenBound : int) 
(currentIteration : int) 
(terminationIteration:int) 
(cleanupIteration : int) 
(newChildThreshold : int) 
(mutationOperations : mutationOperations) =
  let nothing_population = List.nth currentQueue 0 in
  let confirmed_population = List.nth currentQueue 1 in
  let accepted_population = List.nth currentQueue 2 in
  let total_population_size = population_size_across_queues nothing_population confirmed_population accepted_population in
  if currentIteration >= terminationIteration then iTraces
  else
    if currentIteration mod cleanupIteration = 0 || total_population_size >= maxCurrentPopulation then
      fuzzingAlgorithm maxCurrentPopulation (cleaupPopulation currentQueue) iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations
    else
      let currentQueue = 
        [NOTHING (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state nothing_population));
        CONFIRMED (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state confirmed_population));
        ACCEPTED (List.filter (fun x -> List.length (fst x |> fst) <= 10) (extract_child_from_state accepted_population))]
      in
      (* let top_sample = sample_from_percentile_range currentPopulation 0.0 10.0 1 in
      let middle_sample = sample_from_percentile_range currentPopulation 30.0 60.0 2 in
      let bottom_sample = sample_from_percentile_range currentPopulation 80.0 100.0 1 in *)
      (* let newPopulation = top_sample @ middle_sample @ bottom_sample in *)
      let newPopulation = uniform_sample_from_queue currentQueue 10 in
      let selectedMutations = mutationList random_element mutationOperations (List.length newPopulation) in
      let mutatedPopulation = newMutatedSet newPopulation selectedMutations (List.length newPopulation) in
      let (iT, newPopulation) = executeMutatedPopulation mutatedPopulation in
      let newQueue = bucket_oracle currentQueue newPopulation in 
      fuzzingAlgorithm maxCurrentPopulation newQueue (List.append iTraces iT) tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations

let runFuzzer grammar = 
  Random.self_init ();
  let _ = fuzzingAlgorithm 1000 [NOTHING([([], grammar), 0.0]); CONFIRMED([([], grammar), 0.0]); ACCEPTED([([], grammar), 0.0])] [] 100 0 1000 20 100 [Add; Delete; Modify; CrossOver;CorrectPacket;] in
  ()