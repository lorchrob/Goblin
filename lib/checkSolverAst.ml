module SA = SolverAst
module A = Ast
module E = Evaluator

(* Check
  1. The solver_ast is an instance of the grammar (syntactic well-formedness)
    a. The grammar starts at the start symbol
    b. Each non-leaf node in the solver ast, along with its children,
       has some corresponding grammar production rule
  2. Solver_ast respects every semantic constraint in ast (semantic well-formedness)
*)

type verdict =
| Valid
| Violated of string
(* A constraint the evaluator cannot decide, so the term is neither accepted nor
   rejected. Callers should treat this as a gap, not a pass. *)
| Unknown of string

let check_start_symbol: Ast.ast -> SolverAst.solver_ast -> (unit, string) result
= fun ast solver_ast -> match ast, solver_ast with
| A.ProdRule (nt, _, _, _) :: _, SA.Node ((constructor, _, _), _) ->
  if Nt.equal_ci nt (Nt.unstub constructor)
    then Ok ()
  else
  Error (Format.asprintf "Solver AST root constructor '%a' does not match the AST start symbol '%a'" Nt.pp constructor Nt.pp nt)
| A.TypeAnnotation _ :: _, _ -> Utils.crash "Unexpected case in check_start_symbol"
| [], _ -> Error "Grammar is empty"
| _, Leaf _ -> Error "Term is a bare value, not rooted at the start symbol"
| _, StubLeaf _ -> Error "Term is an uncomputed stub, not rooted at the start symbol"
| _, Model _ -> Error "Term is an SMT model, not a grammar term"
| _, Infeasible -> Error "Term reports infeasibility, so there is nothing to check"

(* A derived field constrains the value at its own position, so it checks as an
   equality between that position and the field's definition *)
let constraint_of_derived_field nt expr rhs_idx p =
  A.CompOp (A.NTExpr ([nt, Some rhs_idx, Some 0], p), Eq, expr, p)

(* Constraints whose dot notation references categories absent from this term
   denote Top and hold trivially, so no separate applicability test is needed *)
let check_constraints solver_ast constructor scs rhs_idx =
  let env = { E.node = solver_ast; deps = Nt.StubMap.empty } in
  List.fold_left (fun verdict sc ->
    match verdict with
    | Violated _ -> verdict
    | Valid | Unknown _ ->
      let expr = match sc with
      | A.SmtConstraint (expr, _) -> expr
      | A.DerivedField (nt, expr, p) -> constraint_of_derived_field nt expr rhs_idx p
      (* The attribute's generated child must hold the value its definition computes *)
      | A.AttrDef (attr, expr, p) -> A.CompOp (A.OwnSynthAttr (attr, p), Eq, expr, p)
      in
      (
        match E.holds env expr with
        | Ok true -> verdict
        | Ok false ->
          Violated (Format.asprintf "Semantic constraint %a on constructor '%a' is falsified"
            A.pp_print_semantic_constraint sc Nt.pp constructor)
        | Error e ->
          Unknown (Format.asprintf "Semantic constraint %a on constructor '%a': %a"
            A.pp_print_semantic_constraint sc Nt.pp constructor E.pp_error e)
      )
  ) Valid scs

(* A finished term is closed: nothing is left for a later stage to fill in.
   Placeholders are not a defect, since the Placeholder type has them as its
   values; a misplaced one is caught by the declared type instead. *)
let rec check_closed: SolverAst.solver_ast -> verdict
= fun solver_ast -> match solver_ast with
| Leaf _ -> Valid
| StubLeaf stub ->
  Violated (Format.asprintf "Term contains the uncomputed stub '%a'" Nt.pp (Stub stub))
| Model _ -> Violated "Term contains an SMT model"
| Infeasible -> Violated "Term contains an infeasibility marker"
| Node (_, children) ->
  List.fold_left (fun acc child -> match acc with
  | Violated _ -> acc
  | Valid | Unknown _ -> check_closed child
  ) Valid children

(* The declared type of a symbolic terminal, checked against the value present.
   Nothing else in the pipeline verifies bit-vector widths. *)
let check_value_type constructor ty children =
  match children with
  | [SA.Leaf value] ->
    if A.eq_il_type (Value.ty value) ty then Valid
    else
      Violated (Format.asprintf
        "Value %a at '%a' has type %a, but the grammar declares %a"
        Value.pp value Nt.pp constructor A.pp_print_ty (Value.ty value) A.pp_print_ty ty)
  | [] | _ :: _ :: _ | [SA.Node _] | [SA.StubLeaf _] | [SA.Model _] | [SA.Infeasible] ->
    Unknown (Format.asprintf "Type annotation '%a' does not hold a single value"
      Nt.pp constructor)

(* Nodes the desugaring adds for attributes; the grammar as written does not
   mention them, so rule matching ignores them *)
let grammar_children children =
  List.filter (fun child -> match child with
  | SA.Node ((label, _, _), _) -> not (Nt.is_attribute label)
  | SA.Leaf _ | SA.StubLeaf _ | SA.Model _ | SA.Infeasible -> true
  ) children

(* The production rule option whose right-hand side matches this node's children *)
let matching_rhs children rhss =
  let children = grammar_children children in
  List.find_mapi (fun i rhs -> match rhs with
  | A.StubbedRhs _ -> None
  | A.Rhs (ges, scs, _, _) ->
    if List.length ges != List.length children then None
    else
      if List.for_all2 (fun child ge ->
        match child, ge with
        | _, A.StubbedNonterminal _ -> false
        | SA.Node ((constructor, _, _), _), A.Nonterminal (nt, _, _, _, _) ->
          Nt.equal_ci (Nt.unstub constructor) nt
        (* A nonterminal in the rule must be a node in the term: the engine always
           emits that level, and eliding it would skip the child's declared type *)
        | (SA.Leaf _ | SA.StubLeaf _ | SA.Model _ | SA.Infeasible), A.Nonterminal _ -> false
      ) children ges
      then Some (scs, ges, i) else None
  ) rhss

let worst_of v1 v2 = match v1, v2 with
| Violated _, _ -> v1
| _, Violated _ -> v2
| Unknown _, _ -> v1
| Valid, (Valid | Unknown _) -> v2

(* Each inherited attribute passed at a call site must equal the value held by the
   callee's generated child for that parameter. This is the one part of the
   desugaring the checker would otherwise have to take on trust. *)
let check_inherited_args ast solver_ast constructor ges =
  let constraints = List.concat_map (fun ge -> match ge with
  | A.StubbedNonterminal _ -> []
  | A.Nonterminal (nt, idx1, idx2, args, p) ->
    match A.find_element ast nt with
    | A.TypeAnnotation _ -> []
    | A.ProdRule (Nt.User callee, params, _, _) ->
      List.mapi (fun i arg ->
        let param, _ = List.nth params i in
        A.CompOp (A.NTExpr ([nt, idx1, idx2; Nt.InhAttr (callee, param), None, None], p),
                  Eq, arg, p)
      ) args
    | A.ProdRule ((Nt.SynthAttr _ | Nt.InhAttr _ | Nt.Stub _), _, _, _) -> []
    | exception Not_found -> []
  ) ges in
  check_constraints solver_ast constructor
    (List.map (fun c -> A.SmtConstraint (c, A.pos_of_expr c)) constraints) 0

(* This node's own constraints: those of the type annotation or of the production
   rule option whose right-hand side matches its children *)
(* An inherited attribute's type is declared in its owner's parameter list rather
   than as a top-level annotation, so it has no element of its own to look up *)
let check_inherited_param ast constructor owner attr children =
  match A.find_element ast (Nt.User owner) with
  | A.ProdRule (_, params, _, _) -> (
    match List.assoc_opt attr params with
    | Some ty -> check_value_type constructor ty children
    | None ->
      Violated (Format.asprintf "Nonterminal <%s> does not declare an inherited attribute %s"
        owner attr)
  )
  | A.TypeAnnotation _ ->
    Violated (Format.asprintf "<%s> is a type annotation, so it has no inherited attributes" owner)
  | exception Not_found ->
    Violated (Format.asprintf "Dangling owner <%s> for inherited attribute %s" owner attr)

let check_node: Ast.ast -> SolverAst.solver_ast -> Nt.t -> SolverAst.solver_ast list -> verdict
= fun ast solver_ast constructor children ->
  match constructor with
  | Nt.InhAttr (owner, attr) -> check_inherited_param ast constructor owner attr children
  | Nt.User _ | Nt.SynthAttr _ | Nt.Stub _ ->
  let element = List.find_opt (fun element -> match element with
  | A.TypeAnnotation (nt, _, _, _)
  | A.ProdRule (nt, _, _, _) -> Nt.equal_ci (Nt.unstub constructor) nt
  ) ast in
  match element with
  | None ->
    Violated (Format.asprintf "Dangling constructor identifier %a" Nt.pp (Nt.unstub constructor))
  | Some (A.TypeAnnotation (_, ty, scs, _)) ->
    worst_of (check_value_type constructor ty children)
      (check_constraints solver_ast constructor scs 0)
  | Some (A.ProdRule (_, _, rhss, _)) ->
    match matching_rhs children rhss with
    | None ->
      Violated (Format.asprintf "Could not find an associated production rule for constructor '%a'" Nt.pp constructor)
    | Some (scs, ges, rhs_idx) ->
      worst_of (check_constraints solver_ast constructor scs rhs_idx)
        (check_inherited_args ast solver_ast constructor ges)

let rec check_syntax_semantics: Ast.ast -> SolverAst.solver_ast -> verdict
= fun ast solver_ast -> match solver_ast with
| Node ((constructor, _, _), children) ->
  (* In dpll divide and conquer module, we get an extra nesting of stub and
     concrete NTs for some reason. *)
  let skip_condition = match children with
  | [Node ((constructor2, _, _), _)] -> Nt.equal_ci constructor (Nt.unstub_once constructor2)
  | [] | _ :: _ :: _ | [Leaf _] | [StubLeaf _] | [Model _] | [Infeasible] -> false
  in
  if skip_condition then check_syntax_semantics ast (List.hd children)
  else
    let children_verdict =
      List.fold_left (fun acc child -> match acc with
      | Violated _ -> acc
      | Valid | Unknown _ -> worst_of acc (check_syntax_semantics ast child)
      ) Valid children
    in (
    match children_verdict with
    | Violated _ -> children_verdict
    | Valid | Unknown _ ->
      worst_of children_verdict (check_node ast solver_ast constructor children)
    )
| Leaf _ | StubLeaf _ | Model _ | Infeasible -> Valid

let verdict_of: Ast.ast -> SolverAst.solver_ast -> verdict
= fun ast solver_ast ->
  Utils.debug_print SA.pp_print_solver_ast Format.std_formatter solver_ast;
  match check_start_symbol ast solver_ast with
  | Error msg -> Violated msg
  | Ok () ->
    match check_closed solver_ast with
    | Violated _ as verdict -> verdict
    | Valid | Unknown _ -> check_syntax_semantics ast solver_ast

(* Fails closed: an undecided constraint is reported as an error rather than
   silently accepted *)
let check_solver_ast: Ast.ast -> SolverAst.solver_ast -> (unit, string) result
= fun ast solver_ast -> match verdict_of ast solver_ast with
| Valid -> Ok ()
| Violated msg -> Error msg
| Unknown msg -> Error ("Could not be checked: " ^ msg)
