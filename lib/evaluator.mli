(** Evaluates Goblin expressions against a generated term, for both {!ComputeDeps}
    (derived fields) and {!CheckSolverAst} (checking a term against its grammar). *)

(** A path referencing a category absent from the term denotes [Top], which every
    operator propagates and every constraint accepts, so applicability needs no test. *)
type v =
| Top
| Val of Value.t

type error =
| Unsupported of string * Lexing.position
  (** Construct the evaluator does not implement. Report it as an undecided result,
      not as a violated constraint. *)
| Unspecified of string * Lexing.position
  (** Value is not determined by the semantics, e.g. division by zero, which
      SMT-LIB leaves uninterpreted. *)
| Type_error of string * Lexing.position
| Internal of string * Lexing.position

val pp_error : Format.formatter -> error -> unit

val pos_of_error : error -> Lexing.position

type env = {
  node : SolverAst.solver_ast;
  (** The term the expression is evaluated against; dot-notation paths resolve
      from here. *)
  deps : Ast.semantic_constraint Nt.StubMap.t;
  (** Derived fields still held as stubs, computed on demand when evaluation
      reaches one. *)
  at_annotation : bool;
  (** Whether this is a type annotation's own constraint, whose paths name the
      annotated node. Set it only there; inside a rule it aliases recursion to itself. *)
}

val eval : env -> Ast.expr -> (v, error) result
(** [ubv_to_int] and [sbv_to_int] refuse bit vectors wider than 62 bits with
    [Unsupported], rather than wrapping silently on a native int. *)

val leaves : env -> SolverAst.solver_ast -> (Value.t list, error) result
(** Every leaf value beneath a term, left to right, computing derived fields on
    demand. *)

val compute_stub : env -> Nt.stub -> (Value.t, error) result
(** Computes the derived field a stub stands for. [env.node] must be the stub's
    parent, since that is the rule the definition is written against. *)

val holds : env -> Ast.expr -> (bool, error) result
(** Whether a constraint is satisfied. [Top] counts as satisfied. *)
