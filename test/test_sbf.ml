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

let test_bl () = 
  let input = 
  "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> { length(<AUTH_ALGO>) > 1; length(<STATUS_CODE>) > 5; };

  <STATUS_CODE> :: BitList;
  <AUTH_ALGO> :: BitList;
  "
  in 
  let output = main_pipeline input in 
  check string "test_bl" output "00000000\n"

let test_top_level_ty_annot () = 
  let input = 
  " <STATUS_CODE> :: BitList;
  "
  in 
  let output = main_pipeline input in 
  check string "test_top_level_ty_annot" output "\n"

let test_ty_annot_sc () = 
  let input = 
  " <STATUS_CODE> :: BitList { length(<STATUS_CODE>) > 0; };
  "
  in 
  let output = main_pipeline input in 
  check string "test_ty_annot_sc" output "0\n"

let () = 
  run "My_module" [
      "test_sc", [test_case "Semantic constraint" `Quick test_sc];
      "test_dt", [test_case "Dependent term" `Quick test_dt];
      "test_dc", [test_case "Divide and conquer" `Quick test_dc];
      "test_bl", [test_case "Bit list" `Quick test_bl];
      "test_top_level_ty_annot", [test_case "Top level type annotation" `Quick test_top_level_ty_annot];
      "test_ty_annot_sc", [test_case "Top level type annotation with semantic constraint" `Quick test_ty_annot_sc];
    ]