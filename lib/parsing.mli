val parse : string -> Ast.ast
val parse_sygus : string -> Ast.ast -> (SolverAst.solver_ast, string) result
