(*
The main function takes an AST as input, and returns an "abstracted" AST 
where the dependent (computed) terms are replaced with "stubbed" counterparts 
(ie, opaque hard-coded string constants). 
We perform this step in order to remove the handling of dependent terms from the 
SyGuS encoding. However, we must retain a dependency map, which maps each of 
the hard-coded string constants to its corresponding dependency information 
(in other words, instructions for how to compute it after the SyGuS call).
*)

val abstract_dependencies : TypeChecker.context -> Ast.ast -> (Ast.semantic_constraint Utils.StringMap.t * Ast.ast * TypeChecker.context)  
