open Sbf

(* TODO: 
   1. Allow multiple options for a production rule, either with BNF syntax
      or multiple separate production rules. The former requires extending parser
      but does not require merging production rules for same nonterminal together. 


   1. Collect stubs in some data structure and add them to sygus printing stuff
   2. Fix indexing 
   3. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   4. Syntax check on dot notation, make sure it's unambiguous 
   5. Infer types for nonterminals with production rules 
   6. Make sure each nonterminal has a path to termination


   <A> -> <B> <C> <D> { sc1 } | <E> <F> <G> (no sc) --> 

   <A> -> STUB_A | <E> <F> <G> (no sc)
   and 
   STUB_A -> <B> <C> <D> { sc1 }

   Test 1 with multiple scs at top level, and also one where the scs have to be stubbed

*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
  "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
  <STATUS_CODE> ::= <BV1> <BV2> | <BV2> { <BV2> = 0b0000000000000011; };
  <AUTH_ALGO> ::= <BV1> { <BV1> = 0b0000000000000111; };
  <BV1> :: BitVector(16);
  <BV2> :: BitVector(16);
  "
  )


  