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
  ignore (Pipeline.main_pipeline 
     " <STATUS_CODE> :: BitVector(16);
   "
   )
  