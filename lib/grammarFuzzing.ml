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


type packet_type = COMMIT | CONFIRM | ASSOCIATION_REQUEST | NOTHING

type grammar = ast

type mutation = Add | Delete | Modify | CrossOver | None | CorrectPacket

type mutationOperations = mutation list

type output = CRASH | TIMEOUT | EXPECTED_OUTPUT | UNEXPECTED_OUTPUT | Message of string

type provenance = RawPacket of packet | ValidPacket of packet_type

type child = (provenance list * grammar) * score
type population = child list

type trace = packet list

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


let rec scoreFunction (pktStatus : (provenance * output) list) (mutatedPopulation : population) : ((provenance list list) * population) =
  match pktStatus, mutatedPopulation with
  | [], [] | _, [] -> [], []
  | [], p -> [], p
  | (packet, output_symbol) :: xs, ((old_provenance, current_grammar), score) :: ys ->
    match output_symbol with
    | TIMEOUT | EXPECTED_OUTPUT ->
      if output_symbol = TIMEOUT then
        ((first (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.3) :: (second (scoreFunction xs ys)))
      else 
        ((first (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.1) :: (second (scoreFunction xs ys)))
    | CRASH | UNEXPECTED_OUTPUT ->
      if output_symbol = CRASH then 
        ((old_provenance @ [packet])  :: (first (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.7) :: (second (scoreFunction xs ys)))
      else 
        ((old_provenance @ [packet]) :: (first (scoreFunction xs ys))), (((old_provenance @ [packet], current_grammar), score +. 0.9) :: (second (scoreFunction xs ys)))        
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
let sample_from_percentile_range (pop: population) (lower_percentile: float) (upper_percentile: float) (sample_size: int) : child list =
  let sorted_pop = List.sort (fun (_, score1) (_, score2) -> compare score2 score1) pop in
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


let rec applyMutation (m : mutation) (g : ast) : packet_type * grammar =
  let nt = random_element nonterminals in
  match m with
    Add -> print_endline "\n\nADDING\n\n" ; 
    let added_grammar = first (mutation_add_s1 g nt) in
    pp_print_ast Format.std_formatter added_grammar ;
    NOTHING, (mutate_till_success added_grammar g Add)

  | Delete -> print_endline "\n\nDELETING\n\n" ;
    let deleted_grammar = first (mutation_delete g nt) in
    pp_print_ast Format.std_formatter deleted_grammar ;

    NOTHING, (mutate_till_success deleted_grammar g Delete)

  | Modify -> print_endline "\n\nMODIFYING\n\n" ;
    let modified_grammar = first (mutation_delete g nt) in
    pp_print_ast Format.std_formatter modified_grammar ;

    NOTHING, (mutate_till_success modified_grammar g Modify)

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
        | Some(x) -> pp_print_ast Format.std_formatter x; 
          NOTHING, (mutate_till_success x g CrossOver)
        | None -> (applyMutation CrossOver g)
        )
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
    )
  | None -> NOTHING, g
and 
  mutate_till_success (mutated_grammar : ast) (original_grammar : ast) (mutation_op : mutation) : ast = 
  let sygusOutput = (Pipeline.sygusGrammarToPacket mutated_grammar) in
    match sygusOutput with
    | Ok _ -> mutated_grammar
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
        mutate_till_success updated_mutation original_grammar mutation_op
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
        let canonicalizedGrammar = canonicalize finalGrammar in
          (match canonicalizedGrammar with
          | Some(x) -> pp_print_ast Format.std_formatter x; 
            mutate_till_success x original_grammar mutation_op
          | None -> let tupleMutation = applyMutation CrossOver original_grammar in
            match tupleMutation with
            (_, x) -> x
          )
      | CorrectPacket -> failwith "correctpacket mutation shouldnt have been passed to this func.."
      | None -> original_grammar


let rec newMutatedSet (p:population) (m:mutationOperations) (n:int) : population = 
  match n, p, m with
  | 0, _, _ -> []
  | _, _, [] -> []
  | _, [], _ -> []
  | _, (x::xs), (mu::ms) ->
    let mutated_grammar = applyMutation mu (first x |> second) in
    (match mutated_grammar with
    | (NOTHING, z) -> ((((first x |> first)), z), 0.0) :: (newMutatedSet xs ms (n - 1))
    | (y, z) -> (((first x |> first) @ [(ValidPacket y)], z), 0.0) :: (newMutatedSet xs ms (n - 1))
      (* (((first x |> first), mutated_grammar), 0.0) :: (newMutatedSet xs ms (n - 1)) *)
    )
let rec mutationList sampleFunction (mutationOps:mutationOperations) (n:int): mutation list =
  match n with
    0 -> []
  | _ -> sampleFunction mutationOps :: mutationList sampleFunction mutationOps (n - 1)

let rec sendPacketsToState (p : provenance list) : unit = 
  match p with
    [] -> ()
  | x :: xs -> let _ = callDriver x in sendPacketsToState xs

let sendPacket (c : child) : (provenance * output) =
  let stateTransition = first c |> first in
  let packetToSend = Result.get_ok (Pipeline.sygusGrammarToPacket (first c |> second)) in 
    sendPacketsToState stateTransition ;
    (RawPacket packetToSend, callDriver (RawPacket packetToSend))
  
let executeMutatedPopulation (mutatedPopulation : population) : ((provenance list list) * population) =
  let outputList = List.map sendPacket mutatedPopulation in
    scoreFunction outputList mutatedPopulation

(* Filter population based on standard deviation *)
let getScores (p:population) : score list = List.map second p

let calcMean (s:score list) : float =
  let sum = List.fold_left (+.) 0.0 s in
  sum /. float_of_int (List.length s)

let stdDev (s:score list) : float =
  let m = calcMean s in
  let variance = List.fold_left (fun a x -> a +. (x -. m) ** 2.0) 0.0 s in
  sqrt (variance /. float_of_int (List.length s))


let cleaupPopulation (p: population) : population =
  (* check scores for staleness, remove population that has scores with little to no SD, 
     ignore 0.0 when checking for staleness *)
  let s = getScores p in 
  let sd = stdDev s in
  List.filter (fun (_, sc) -> sc = 0.0 || sc > sd) p
(* END CLEANUP *)

let rec fuzzingAlgorithm 
(maxCurrentPopulation : int) 
(currentPopulation : population) 
(iTraces : provenance list list)
(tlenBound : int) 
(currentIteration : int) 
(terminationIteration:int) 
(cleanupIteration : int) 
(newChildThreshold : int) 
(mutationOperations : mutationOperations) =
  if currentIteration >= terminationIteration then iTraces
  else
    if currentIteration mod cleanupIteration = 0 || List.length currentPopulation >= maxCurrentPopulation then
      fuzzingAlgorithm maxCurrentPopulation (cleaupPopulation currentPopulation) iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations
    else
      let currentPopulation = List.filter (fun x -> List.length (first x |> first) < 10) currentPopulation in
      let top_sample = sample_from_percentile_range currentPopulation 0.0 10.0 1 in
      let middle_sample = sample_from_percentile_range currentPopulation 30.0 60.0 2 in
      let bottom_sample = sample_from_percentile_range currentPopulation 80.0 100.0 1 in
      let newPopulation = top_sample @ middle_sample @ bottom_sample in
      let selectedMutations = mutationList random_element mutationOperations (List.length newPopulation) in
      let mutatedPopulation = newMutatedSet newPopulation selectedMutations (List.length newPopulation) in
      let (iT, newPopulation) = executeMutatedPopulation mutatedPopulation in
      fuzzingAlgorithm maxCurrentPopulation (List.append newPopulation currentPopulation) (List.append iTraces iT) tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations

let runFuzzer grammar = 
  Random.self_init ();
  let _ = fuzzingAlgorithm 1000 [(([], grammar), 0.0); (([], grammar), 0.0); (([], grammar), 0.0);] [] 100 0 1000 20 100 [Add; Delete; Modify; CrossOver;CorrectPacket;] in
  ()