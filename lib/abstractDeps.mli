(** Replace dependent (derived) terms with stubs, returning a map from each stub 
    to the derived field that computes it after solving *)
val abstract_dependencies : TypeChecker.context -> Ast.ast -> (Ast.semantic_constraint Nt.StubMap.t * Ast.ast * TypeChecker.context)
