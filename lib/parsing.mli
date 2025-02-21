val parse : string -> Ast.ast
val parse_sygus : string -> Ast.ast -> (SygusAst.sygus_ast, string) result
