(*
Main function takes a solver AST as input, along with the input AST, 
and checks whether or not the solver AST is valid (that is, in the 
language of the input AST, also accounting for semantic constraints).

Currently, this module is independent from the rest of the code. 
In the future, this will be used to verify the correctness of LLM-produced outputs.
*)

val check_solver_ast : Ast.ast -> SolverAst.solver_ast -> (unit, string) result
