val parse : string -> Ast.ast
val parse_solver : string -> Ast.ast -> (SolverAst.solver_ast, string) result
