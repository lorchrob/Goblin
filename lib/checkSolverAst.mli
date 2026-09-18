(*
Main function takes a solver AST as input, along with the input AST,
and checks whether or not the solver AST is valid (that is, in the
language of the input AST, also accounting for semantic constraints).

Currently, this module is independent from the rest of the code.
In the future, this will be used to verify the correctness of LLM-produced outputs.
*)

type verdict =
| Valid
| Violated of string
(** A constraint the evaluator cannot decide, so the term is neither accepted nor
    rejected. Callers should treat this as a gap in the oracle, not a pass. *)
| Unknown of string

val verdict_of : Ast.ast -> SolverAst.solver_ast -> verdict

(** Fails closed: [Unknown] is reported as an error rather than silently accepted. *)
val check_solver_ast : Ast.ast -> SolverAst.solver_ast -> (unit, string) result
