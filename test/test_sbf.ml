open Sbf.Pipeline
open Alcotest

(* Semantic constraint example *)
let test_sc () =
  let input = 
   "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
      { <AUTH_ALGO> = int_to_bitvector(16, 12); };

      <STATUS_CODE> :: BitVector(16);
      <AUTH_ALGO> :: BitVector(16);
   "
  in
  let output = main_pipeline input in
  check string "test_sc" output "00000000000011000000000000000000\n"

(* Dependent term calculation example *)
let test_dt () =
  let input = 
    "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
        { <AUTH_ALGO> <- int_to_bitvector(16, 12); };

        <STATUS_CODE> :: BitVector(16);
        <AUTH_ALGO> :: BitVector(16);
    "
  in 
  let output = main_pipeline input in
  check string "test_dt" output "00000000000011000000000000000000\n"

(* Divide and conquer example *)
let test_dc () = 
  let input = 
  "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
  <STATUS_CODE> ::= <BV> { <BV> = 0b0000000000000000; };
  <AUTH_ALGO> ::= <BV> { <BV> = 0b0000000000000001; };
  <BV> :: BitVector(16);
  "
  in 
  let output = main_pipeline input in 
  check string "test_dc" output "00000000000000010000000000000000\n"

let () = 
  run "My_module" [
      "test_sc", [test_case "Semantic constraint example" `Quick test_sc];
      "test_dt", [test_case "Dependent term example" `Quick test_dt];
      "test_dc", [test_case "Divide and conquer example" `Quick test_dc];
    ]