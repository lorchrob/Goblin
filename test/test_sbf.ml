open Sbf.Pipeline
open Alcotest

(* TODO
  1. For "infeasible" tests, create non-infeasible counterpart
  2. Add tests from main.ml

*)

let test_another_ambiguous_reference_1 () =
  let input = 
    "
    <S> ::= <A> { <A>.<B>.<D> > <A>.<B>.<D>; };
    <A> ::= <B> <B>;
    <B> ::= <D> <D>;
    <D> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let test_another_ambiguous_reference_2 () =
  let input = 
    "
    <S> ::= <A> { <A>.<B>.<D> > <A>.<B>.<D>; };
    <A> ::= <B> | <B> <D>;
    <B> ::= <D> <D>;
    <D> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let test_another_ambiguous_reference () =
  let input = 
    "
    <S> ::= <A> { <A>.<B>.<D> > <A>.<C>.<D>; };
    <A> ::= <B> <B> <C> <C>;
    <B> ::= <D>;
    <C> ::= <D>;
    <D> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_another_ambiguous_reference" output "00-1-1\n"

let test_dot_notation_2 () =
  let input = 
    "
    <S> ::= <A> { <A>.<B>.<C> > <A>.<B>.<D>; };
    <A> ::= <B>;
    <B> ::= <C> <D>;
    <C> :: Int;
    <D> :: Int; 
  "
  in
  let output = main_pipeline input in
  check string "test_dot_notation_2" output "0-1\n"

let test_cyclic_dependencies () =
  let input = 
    "
    <S> ::= <A> <B> <C> <D> { <A> <- <B>; <B> <- <A>; <C> <- <D>; <D> <- <C>; };
    <A> :: Int;
    <B> :: Int;
    <C> :: Int; 
    <D> :: Int;
"
  in
  let output = main_pipeline input in
  check string "test_cyclic_dependencies" output "0000\n"

let test_horizontal_ambiguous_reference_1 () =
  let input = 
    "
    <S> ::= <A> { <A>.<B> > <A>.<C>; };
    <A> ::= <B> <B> <C> <C>;
    <B> :: Int;
    <C> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_horizontal_ambiguous_reference_1" output "00-1-1\n"

let test_vertical_ambiguous_reference_1 () =
  let input = 
    "
    <S> ::= <A> <E> <B> { <A>.<B>.<D> > <E>.<B>.<C>; };
    <E> ::= <B>;
    <A> ::= <B> <C>;
    <B> ::= <C> <D>;
    <C> :: Int;
    <D> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_vertical_ambiguous_reference_1" output "000-1000\n"

let test_vertical_ambiguous_reference_2 () =
  let input = 
    "
    <S> ::= <A> <B> { <A>.<B>.<D> > <A>.<C>; };
    <A> ::= <B> <C>;
    <B> ::= <C> <D>;
    <C> :: Int;
    <D> :: Int;
  "
  in
  let output = main_pipeline input in
  check string "test_vertical_ambiguous_reference_2" output "00-100\n"

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

let test_placeholder () =
  let input = 
    "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
       { <AUTH_ALGO> <- \"placeholder\"; };
 
       <STATUS_CODE> :: BitVector(16);
       <AUTH_ALGO> :: String;
    "
  in
  let output = main_pipeline input in
  check string "test_sc" output "placeholder0000000000000000\n"

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

let test_dt2 () =
  let input = 
    "
    <SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
    <STATUS_CODE> :: BitVector(16);
    <AUTH_ALGO> :: BitVector(16) { <AUTH_ALGO> <- 0b0000000000000111; };
    "
  in
  let output = main_pipeline input in
  check string "test_sc" output "00000000000001110000000000000000\n"

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
  check bool "test_bl" (String.length output >= 8) true

  (* Should cause a failure *)
(* let test_top_level_ty_annot () = 
  let input = 
    " <STATUS_CODE> :: BitList;
    "
  in 
  let output = main_pipeline input in 
  check string "test_top_level_ty_annot" output "\n" *)

  (* Should cause a failure *)
(* let test_ty_annot_sc () = 
  let input = 
    " <STATUS_CODE> :: BitList { length(<STATUS_CODE>) > 0; };
    "
  in 
  let output = main_pipeline input in 
  check string "test_ty_annot_sc" output "0\n" *)

let test_ty_annot_sc2 () = 
  let input = 
    "
    <SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
    <STATUS_CODE> :: BitVector(16);
    <AUTH_ALGO> :: BitVector(16) { <AUTH_ALGO> = 0b0000000000000111; };
    "
  in 
  let output = main_pipeline input in 
  check string "test_ty_annot_sc2" output "00000000000001110000000000000000\n"

let test_mult_prod_rules () = 
  let input = 
    "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
    <STATUS_CODE> ::= <BV1> <BV2> | <BV2> { <BV2> = 0b0000000000000011; };
    <AUTH_ALGO> ::= <BV1> { <BV1> = 0b0000000000000111; };
    <BV1> :: BitVector(16);
    <BV2> :: BitVector(16);
    "
  in 
  let output = main_pipeline input in 
  check string "test_mult_prod_rules" output "00000000000001110000000000000011\n"

let test_bv_len () = 
  let input = 
    "
    <S> ::= <A> { <A> <- int_to_bitvector(8, length(0b00000)); } ;
    <A> :: BitVector(8);
    "
  in 
  let output = main_pipeline input in 
  check string "test_bv_len" output "00000101\n"

let test_dt3 () = 
  let input = 
    "
    <S> ::= <A> <B> { <A> <- <B>; };

    <A> :: BitVector(2); 
    <B> :: BitVector(2) { <B> = 0b01; } ;
    "
  in 
  let output = main_pipeline input in 
  check string "test_dt3" output "0101\n" 

let test_dt4 () = 
  let input = 
    "<S> ::= <A> <B> <C> { <A> <- <B>; <B> <- 3;};

    <A> :: Int;
    <B> :: Int;
    <C> :: Int;
  "
  in 
  let output = main_pipeline input in 
  check string "test_dt4" output "330\n" 

let test_dt5 () = 
  let input = 
    "<S> ::= <A> <B> <C> { <A> <- <B>; <B> <- <C>;} ; 
 
    <A> :: BitVector(2) ;
    <B> :: BitVector(2);
    <C> :: BitVector(2) { <C> = 0b01; };
    "
  in 
  let output = main_pipeline input in 
  check string "test_dt4" output "010101\n" 

let test_dt6 () = 
  let input = 
    "<S> ::= <A> <B> <C> { <A> <- <B>; <B> <- <C>;} ; 
  
    <A> :: BitVector(2) ;
    <B> :: BitVector(2);
    <C> :: BitVector(2) { <C> <- 0b01; };
    "
  in 
  let output = main_pipeline input in 
  check string "test_dt4" output "010101\n" 

let test_recombine () =
  let input = 
    "
<SAE_PACKET> ::= <COMMIT> ;
    <COMMIT> ::= <AUTH_ALGO> <REJECTED_GROUPS>
       {<AUTH_ALGO> <- int_to_bitvector(16, 3);};

    <AUTH_ALGO> :: BitVector(16);
    
    <REJECTED_GROUPS> ::= <RG_ID_LENGTH>  <RG_ID_LIST> 
    { <RG_ID_LENGTH> <- int_to_bitvector(8, length(<RG_ID_LIST>)); };
    
    <RG_ID_LENGTH>   :: BitVector(8); 
    
    <RG_ID_LIST> ::= <RG_ID> | <RG_ID> <RG_ID_LIST>;
     
    <RG_ID> :: BitVector(8);
    "
  in 
  let output = main_pipeline input in 
  check string "test_recombine" output "00000000000000110000100000000000\n" 
  
let test_dynamic_typing () = 
  let input = 
    "
    <S> ::= <A> <B> { <A> <- length(<B>); };
    <B> ::= <C> <D>;
    <C> :: BitList { length(<C>) > 0; }; 
    <D> :: BitVector(8);
    <A> :: Int;
    "
  in 
  let output = String.trim (main_pipeline input) in
  check bool "test_dynamic_typing" (String.length output >= 10) true

let test_dot_notation () = 
  let input = 
    "
    <S> ::= <A> { <A>.<B> + 3 < 0; <A>.<D> + 3 < 0; }; 
    <A> ::= <B> <C> | <D>;
    <B> :: Int;
    <C> :: Int;
    <D> :: Int;
    "
  in 
  let output = String.trim (main_pipeline input) in
  check string "test_dot_notation" output "-40" 

let () = 
  run "My_module" [
    "test_sc", [test_case "Semantic constraint" `Quick test_sc];
    "test_placeholder", [test_case "Placeholder" `Quick test_placeholder];
    "test_dt", [test_case "Dependent term" `Quick test_dt];
    "test_dt2", [test_case "Dependent term 2" `Quick test_dt2];
    "test_dc", [test_case "Divide and conquer" `Quick test_dc];
    "test_bl", [test_case "Bit list" `Quick test_bl];
    "test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick test_mult_prod_rules];
    "test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick test_ty_annot_sc2];
    "test_bv_len", [test_case "Top length function on bitvector" `Quick test_bv_len];
    "test_dt3", [test_case "Dependent term 3" `Quick test_dt3];
    "test_dt4", [test_case "Dependent term 4" `Quick test_dt4];
    "test_dt5", [test_case "Dependent term 5" `Quick test_dt5];
    "test_dt6", [test_case "Dependent term 6" `Quick test_dt6];
    "test_dynamic_typing", [test_case "Dynamic typing" `Quick test_dynamic_typing];
    "test_recombine", [test_case "Recombine" `Quick test_recombine];
    "test_dot_notation", [test_case "Test dot notation" `Quick test_dot_notation];
    "test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick test_vertical_ambiguous_reference_1];
    "test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick test_vertical_ambiguous_reference_2];
    "test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick test_horizontal_ambiguous_reference_1];
    "test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick test_cyclic_dependencies];
    "test_dot_notation_2", [test_case "test_dot_notation_2" `Quick test_dot_notation_2];
    "test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick test_another_ambiguous_reference];
    "test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick test_another_ambiguous_reference_1];
    "test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick test_another_ambiguous_reference_2];
  ]