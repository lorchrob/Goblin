(*
The main function takes an AST as input, and returns an "abstracted" AST 
where the dependent (computed) terms are replaced with stubs. 
We perform this step in order to remove the handling of dependent terms from the 
constraint solving. However, we must retain a dependency map, which maps each 
stub to its corresponding dependency information 
(in other words, instructions for how to compute it after solving).
*)

val abstract_dependencies : TypeChecker.context -> Ast.ast -> (Ast.semantic_constraint Nt.StubMap.t * Ast.ast * TypeChecker.context)
