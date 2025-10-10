let replace_stub: string -> SolverAst.solver_ast list -> SolverAst.solver_ast option
= fun possible_stub solver_asts ->
  List.find_opt (fun solver_ast -> match solver_ast with 
  | SolverAst.IntLeaf _ | BVLeaf _ | BLLeaf _ | VarLeaf _ 
  | BoolLeaf _ | StrLeaf _ | SetLeaf _ | UnitLeaf -> false 
  | Node ((constructor, _), _) -> 
    (* To compare stubs, we only need the stub ID prefix "_stubN"*)
    Utils.extract_base_name possible_stub = Utils.extract_base_name constructor
    || 
    (* For type annotations, we don't use a stub ID *)
    possible_stub = constructor
  ) solver_asts

(* Invariant: First element of solver_asts is the combined AST *)
let rec recombine: SolverAst.solver_ast list -> SolverAst.solver_ast 
= fun solver_asts -> 
  match solver_asts with 
| [] -> assert false
| SetLeaf _ :: _ | IntLeaf _ :: _ | BVLeaf _ :: _ 
| BLLeaf _ :: _ | BoolLeaf _ :: _ | StrLeaf _ :: _ | UnitLeaf :: _ -> List.hd solver_asts
| Node (constructor, children) :: solver_asts ->
  let children = List.map (fun solver_ast -> recombine (solver_ast :: solver_asts)) children in
  Node (constructor, children)

(* Might need to recurse on what is currently a "_" in this pattern match *)
| VarLeaf possible_stub :: _ -> 
  match replace_stub possible_stub solver_asts with 
  | Some solver_ast -> recombine (solver_ast :: solver_asts)
  | None -> List.hd solver_asts
 
