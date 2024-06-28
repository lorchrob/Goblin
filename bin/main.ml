open Sbf

(* TODO: 
   1. Collect stubs in some data structure and add them to sygus printing stuff
   2. Fix indexing 
   3. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   4. Syntax check on dot notation, make sure it's unambiguous 
   5. Infer types for nonterminals with production rules 
   6. Make sure each nonterminal has a path to termination
*)

(* Main function *)
let () = 
  let _ = Pipeline.main_pipeline "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
  { <AUTH_ALGO> = int_to_bitvector(16, 12); };

  <STATUS_CODE> :: BitVector(16);
  <AUTH_ALGO> :: BitVector(16);
" in 
  ()
  