(** Evaluates Goblin expressions against a generated term. Used both to compute
    derived fields (see {!ComputeDeps}) and to check a term against its grammar
    (see {!CheckSolverAst}). *)

(** A dot-notation path referencing a syntactic category absent from the term
    denotes [Top], which propagates through every operator and satisfies any
    constraint, so no separate applicability test is needed. *)
type v =
| Top
| Val of Value.t

type error =
(** Construct the evaluator does not implement. Callers should report this as an
    undecided result, not as a violated constraint. *)
| Unsupported of string * Lexing.position
(** Value is not determined by the semantics, e.g. division by zero, which
    SMT-LIB leaves uninterpreted. *)
| Unspecified of string * Lexing.position
| Type_error of string * Lexing.position
| Internal of string * Lexing.position

val pp_error : Format.formatter -> error -> unit

val pos_of_error : error -> Lexing.position

type env = {
  (** The term the expression is evaluated against; dot-notation paths resolve
      from here. *)
  node : SolverAst.solver_ast;
  (** Derived fields still held as stubs, computed on demand when evaluation
      reaches one. *)
  deps : Ast.semantic_constraint Nt.StubMap.t;
}

val eval : env -> Ast.expr -> (v, error) result

(** Every leaf value beneath a term, left to right, computing derived fields on
    demand. *)
val leaves : env -> SolverAst.solver_ast -> (Value.t list, error) result

(** Computes the derived field a stub stands for. [env.node] must be the stub's
    parent, since that is the rule the definition is written against. *)
val compute_stub : env -> Nt.stub -> (Value.t, error) result

(** Whether a constraint is satisfied. [Top] counts as satisfied. *)
val holds : env -> Ast.expr -> (bool, error) result
