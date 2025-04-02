(*
Main function takes a SyGuS AST as input, along with the input AST, 
and checks whether or not the SyGuS AST is valid (that is, in the 
language of the input AST, also accounting for semantic constraints).

Currently, this module is independent from the rest of the code. 
In the future, this will be used to verify the correctness of LLM-produced outputs.
*)

val check_sygus_ast : Ast.ast -> SygusAst.sygus_ast -> (unit, string) result
