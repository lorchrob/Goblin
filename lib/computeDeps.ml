module SA = SolverAst
module A = Ast
module E = Evaluator

let (let*) = Res.(>>=)

(* Derived fields are held as stubs during the search, since their values are
   computed outside the solver. This replaces each stub with its value. *)

(* A derived field's definition is written against the rule that defines it, which
   is the rule applied at the stub's parent, so the parent is the evaluation point *)
let rec compute_deps_res: A.semantic_constraint Nt.StubMap.t -> SA.solver_ast ->
  (SA.solver_ast, E.error) result
= fun deps solver_ast -> match solver_ast with
| Node (label, children) ->
  let* children = Res.seq (List.map (fun child -> match child with
  | SA.Node (child_label, [SA.StubLeaf stub]) when Nt.StubMap.mem stub deps ->
    let* value = E.compute_stub { E.node = solver_ast; deps } stub in
    Ok (SA.Node (child_label, [SA.Leaf value]))
  | SA.Node _ -> compute_deps_res deps child
  | SA.Leaf _ | SA.StubLeaf _ | SA.Model _ | SA.Infeasible -> Ok child
  ) children) in
  Ok (SA.Node (label, children))
| Leaf _ | StubLeaf _ | Model _ | Infeasible -> Ok solver_ast

(* A derived field that cannot be computed leaves no term to emit, so this is fatal *)
let compute_deps: A.semantic_constraint Nt.StubMap.t -> SA.solver_ast -> SA.solver_ast
= fun deps solver_ast -> match compute_deps_res deps solver_ast with
| Ok solver_ast -> solver_ast
| Error e ->
  Utils.error (Format.asprintf "Could not compute a derived field: %a" E.pp_error e)
    (E.pos_of_error e)
