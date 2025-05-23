val pp_print_ast : Format.formatter -> (TypeChecker.context * Ast.semantic_constraint Utils.StringMap.t * Ast.ast) -> unit 
val call_sygus : TypeChecker.context -> Ast.semantic_constraint Utils.StringMap.t -> Ast.ast -> string
val pp_print_expr: ?nt_prefix:string -> TypeChecker.context -> Format.formatter -> Ast.expr -> unit
val pp_print_nt_helper: Format.formatter -> string * int option -> unit
val pp_print_ty: Format.formatter -> Ast.il_type -> unit 