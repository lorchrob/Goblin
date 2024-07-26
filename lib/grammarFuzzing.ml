open Ast

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

let read_from_file filename =
  let ic = open_in filename in
  try
    let line = input_line ic in
    close_in ic;
    Some line
  with End_of_file ->
    close_in ic;
    None

let write_to_file filename msg =
  let oc = open_out filename in
  output_string oc msg;
  close_out oc

let clear_file filename =
  let oc = open_out filename in
  close_out oc  (* Opens and immediately closes the file to clear its content *)

let wait_for_python_response response_file =
  let rec loop () =
    match read_from_file response_file with
    | Some response ->
        (* Clear the file after reading *)
        clear_file response_file;
        response
    | None ->
        sleep 1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()

let callDriver x =
  let message_file = "../message.txt" in
  let response_file = "../response.txt" in

  (* Write x to the message file *)
  write_to_file message_file x;

  (* Wait for the Python process to write a response and return it *)
  wait_for_python_response response_file
  
type packet = bytes 
type score = float 

type grammar = ast

type mutation = Add | Delete | Modify | CrossOver | None

type mutationOperations = mutation list

type output = CRASH | TIMEOUT | EXPECTED_OUTPUT | UNEXPECTED_OUTPUT

type child = (packet list * grammar) * score
type population = child list

type trace = packet list

type traceSet = trace list

(* type iterationCount = int  *)

(* type childSet = child list *)

let first (tuple: ('a * 'b)) : 'a =
  match tuple with
  (t1, _) -> t1
;;

let second (tuple: ('a * 'b)) : 'b =
  match tuple with
  (_, t2) -> t2
;;


let rec scoreFunction (pktStatus : (packet * output) list) (mutatedPopulation : population) : (trace list * population) =
  match pktStatus, mutatedPopulation with
    [], [] -> [], []
  | status :: statuses, c :: remainingPopulation ->
    match status with
      (_, CRASH) -> let thisScore : score = (second c) +. 0.7 in
      let newPackets : trace = (first c |> first) in
      let iTraces : trace list = [newPackets @ [(first status)]] @ (first (scoreFunction statuses remainingPopulation))  in
      let updatedChildren : population = ((newPackets @ [(first status)], first c |> second), thisScore) :: (second (scoreFunction statuses remainingPopulation)) in
      (iTraces, updatedChildren)
    | (_, TIMEOUT) -> let thisScore : score = (c |> second) +. 0.5 in
      let newPackets : trace = (first c |> first) in
      let updatedChildren : population = ((newPackets @ [(first status)], first c |> second), thisScore) :: (second (scoreFunction statuses remainingPopulation)) in
      ((first (scoreFunction statuses remainingPopulation)), updatedChildren)
    | (_, EXPECTED_OUTPUT) -> let thisScore : score = (second c) +. 0.1 in
      let newPackets : trace = (first c |> first) in
      let updatedChildren : population = ((newPackets @ [(first status)], first c |> second), thisScore) :: (second (scoreFunction statuses remainingPopulation)) in
      ((first (scoreFunction statuses remainingPopulation)), updatedChildren)
    | (_,_) ->
      let thisScore : score = (second c) +. 0.9 in
      let newPackets : trace = (first c |> first) in
      let iTraces : trace list = [newPackets @ [(first status)]] @ (first (scoreFunction statuses remainingPopulation))  in
      let updatedChildren : population = ((newPackets @ [(first status)], first c |> second), thisScore) :: (second (scoreFunction statuses remainingPopulation)) in
      (iTraces, updatedChildren)

    
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

let applyMutation (m:mutation) (g :grammar) : grammar =
  match m with
    Add -> g
  | Delete -> g
  | Modify -> g
  | CrossOver -> g
  | None -> g

let rec newMutatedSet (p:population) (m:mutationOperations) (n:int) : population = 
  match n, p, m with
  | 0, _, _ -> p
  | _, (x::xs), (mu::ms) -> (((first x |> first), (applyMutation mu (first x |> second))), 0.0) :: (newMutatedSet xs ms (n - 1))
  | _ -> failwith "grammarFuzzing.ml incomplete pattern matching"

let rec mutationList sampleFunction (mutationOps:mutationOperations) (n:int): mutation list =
  match n with
    0 -> []
  | _ -> sampleFunction mutationOps :: mutationList sampleFunction mutationOps (n - 1)

let pythonstdIn _ = assert false 

let callDriver packet : output = pythonstdIn packet

let rec sendPacketsToState (p : packet list) : unit = 
  match p with
    [] -> ()
  | x :: xs -> let _ = callDriver x in sendPacketsToState xs

let sendPacket (c:child) : packet * output =
  let stateTransition = first c |> first in
    let packetToSend = Pipeline.sygusGrammarToPacket (first c |> second) in sendPacketsToState stateTransition ;
    (packetToSend, callDriver packetToSend)
    
let executeMutatedPopulation (mutatedPopulation : population) : (trace list * population) =
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
(iTraces : trace list)
(tlenBound : int) 
(currentIteration : int) 
(terminationIteration) 
(cleanupIteration : int) 
(newChildThreshold : int) 
(mutationOperations : mutationOperations) : trace list =
  if currentIteration >= terminationIteration then iTraces
  else
    if currentIteration mod cleanupIteration = 0 || List.length currentPopulation >= maxCurrentPopulation then
      fuzzingAlgorithm maxCurrentPopulation (cleaupPopulation currentPopulation) iTraces tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations
    else
      let top_sample = sample_from_percentile_range currentPopulation 0.0 10.0 1 in
      let middle_sample = sample_from_percentile_range currentPopulation 30.0 60.0 2 in
      let bottom_sample = sample_from_percentile_range currentPopulation 80.0 100.0 1 in
      let newPopulation = top_sample @ middle_sample @ bottom_sample in
      let selectedMutations = mutationList random_element mutationOperations (List.length newPopulation) in
      let mutatedPopulation = newMutatedSet newPopulation selectedMutations (List.length newPopulation) in
      let (iT, newPopulation) = executeMutatedPopulation mutatedPopulation in
      fuzzingAlgorithm maxCurrentPopulation (List.append newPopulation currentPopulation) (List.append iTraces iT) tlenBound (currentIteration + 1) terminationIteration cleanupIteration newChildThreshold mutationOperations
