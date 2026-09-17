(* Find the subproblem solution that replaces `stub` *)
let replace_stub: Nt.stub -> SolverAst.solver_ast list -> SolverAst.solver_ast option
= fun stub solver_asts ->
  List.find_opt (fun solver_ast -> match solver_ast with 
  | SolverAst.Leaf _ | StubLeaf _ | Model _ | Infeasible -> false 
  | Node ((Stub stub', _, _), _) -> Nt.equal_stub stub stub'
  (* Subproblems for type annotations are rooted at the nonterminal itself *)
  | Node ((constructor, _, _), _) -> Nt.equal_ci constructor stub.stands_for
  ) solver_asts

(* Invariant: First element of solver_asts is the combined AST *)
let rec recombine: SolverAst.solver_ast list -> SolverAst.solver_ast 
= fun solver_asts -> 
  match solver_asts with 
| [] -> assert false
| (Leaf _ | Model _ | Infeasible) :: _ -> List.hd solver_asts
| Node (constructor, children) :: solver_asts ->
  let children = List.map (fun solver_ast -> recombine (solver_ast :: solver_asts)) children in
  Node (constructor, children)
| StubLeaf stub :: _ -> 
  match replace_stub stub solver_asts with 
  | Some solver_ast -> recombine (solver_ast :: solver_asts)
  | None -> List.hd solver_asts
