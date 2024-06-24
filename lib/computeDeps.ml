let rec compute_deps: Ast.semantic_constraint Utils.StringMap.t -> SygusAst.sygus_ast -> SygusAst.sygus_ast 
= fun dep_map sygus_ast -> match sygus_ast with
| VarLeaf _ -> assert false 
| Node (constructor, subterms) -> 
  Node (constructor, List.map (compute_deps dep_map) subterms)
| BVLeaf _ | BLLeaf _ | IntLeaf _ -> sygus_ast