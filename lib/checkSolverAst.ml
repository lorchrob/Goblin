module SA = SolverAst
module A = Ast
module R = Res 

let (let*) = Res.(>>=)

(* Check
  1. The solver_ast is an instance of the grammar (syntactic well-formedness)
    a. The grammar starts at the start symbol 
    b. Each non-leaf node in the solver ast, along with its children, 
       has some corresponding grammar production rule 
  2. Solver_ast respects every semantic constraint in ast (semantic well-formedness)
*)

(* Hacky helper function because in the sygus implementation, we use the generated 
   constructor names in the solver ast. *)

let check_start_symbol: Ast.ast -> SolverAst.solver_ast -> (unit, string) result 
= fun ast solver_ast -> match ast, solver_ast with 
| A.ProdRule (nt, _, _, _) :: _, SA.Node ((constructor, _, _), _) -> 
  if Nt.equal_ci nt (Nt.unstub constructor) 
    then Ok () 
  else 
  Error (Format.asprintf "Solver AST root constructor '%a' does not match the AST start symbol '%a'" Nt.pp constructor Nt.pp nt)
| A.TypeAnnotation _ :: _, _ -> Utils.crash "Unexpected case in check_start_symbol"
| _ -> Error "Solver AST root node is a leaf node"

let rec is_nt_applicable: 
  SolverAst.solver_ast -> (Nt.t * int option * int option) list -> bool 
= fun solver_ast nt -> match solver_ast, nt with
  | Node (_, children), head :: tail -> 
    let child = List.find_opt (fun child -> match child with 
    | SA.Node (constructor, _) -> constructor = head
    | _ -> false
    ) children in 
    (match child with 
    | None -> false 
    | Some child -> is_nt_applicable child tail
    )
  | _ -> true

let is_sc_applicable: Ast.expr -> SolverAst.solver_ast -> bool 
= fun expr solver_ast -> 
  let nts = A.get_nts_from_expr2 expr in
  let nts_are_applicable = List.map (is_nt_applicable solver_ast) nts in 
  List.for_all (fun a -> a) nts_are_applicable

let handle_scs ast solver_ast constructor element scs rhs_idx = 
  let scs' = List.map (fun sc -> match sc with 
  | A.SmtConstraint (expr, p) -> 
    if is_sc_applicable expr solver_ast || (* type annotation constraints are always applicable *)
       match element with | A.TypeAnnotation _ -> true | A.ProdRule _ -> false
    then (
      (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is applicable in %a"
        A.pp_print_expr expr
        SA.pp_print_solver_ast solver_ast
        );
      ComputeDeps.evaluate solver_ast ast element expr)
    else (
      (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is not applicable in %a"
        A.pp_print_expr expr
        SA.pp_print_solver_ast solver_ast
        );
      [BConst (true, p)]) (* If sc is not applicable, it trivially holds *)
  | DerivedField (nt, expr, p) -> 
    (* TODO: Should `Some 0` be hardcoded? *)
    if is_sc_applicable expr solver_ast || (* type annotation constraints are always applicable *)
       match element with | A.TypeAnnotation _ -> true | A.ProdRule _ -> false then (
    let expr = A.CompOp (NTExpr ([nt, Some rhs_idx, Some 0], p), Eq, expr, p) in
    (if !Flags.debug then Format.fprintf Format.std_formatter "Dependency %a is applicable in %a"
      A.pp_print_expr expr
      SA.pp_print_solver_ast solver_ast
      );
    ComputeDeps.evaluate solver_ast ast element expr
    ) else (
      (if !Flags.debug then Format.fprintf Format.std_formatter "Dependency %a is not applicable in %a"
        A.pp_print_expr expr
        SA.pp_print_solver_ast solver_ast
        );
      [BConst (true, p)] (* If sc is not applicable, it trivially holds *)
    )
  | AttrDef _ -> assert false
  ) scs in
  let b = List.exists (fun sc -> match sc with 
  | [A.BConst (false, _)] -> true 
  | [BConst (true, _)] -> false 
  | _ -> Utils.crash "Unexpected pattern in check_syntax_semantics"
  ) scs' in
  if b then 
    let i = List.find_index (fun sc -> match sc with 
  | [A.BConst (false, _)] -> true 
  | [BConst (true, _)] -> false 
  | _ -> Utils.crash "Unexpected pattern in check_syntax_semantics"
  ) scs' in 
    let failed_sc = List.nth scs (Option.get i) in
    let msg = Format.asprintf "Semantic constraint %a on constructor '%a' is falsified" 
      A.pp_print_semantic_constraint failed_sc 
      Nt.pp constructor 
    in
    Error msg else
  Ok ()

let rec check_syntax_semantics: Ast.ast -> SolverAst.solver_ast -> (unit, string) result 
= fun ast solver_ast -> match solver_ast with 
  | Node ((constructor, _, _), children) -> 
    (* In dpll divide and conquer module, 
       we get an extra nesting of stub and concrete NTs 
       for some reason. *)
    let skip_condition = 
      match children with 
      | [Node ((constructor2, _, _), _)] ->
        Nt.equal_ci constructor (Nt.unstub_once constructor2)
      | _ -> false
    in
    if skip_condition then check_syntax_semantics ast (List.hd children) else
      
    let* _ = R.seq (List.map (check_syntax_semantics ast) children) in
    (* Find this node's corresponding AST element *) 
    let element = List.find_opt (fun element -> match element with
    | A.TypeAnnotation (nt, _, _, _) 
    | A.ProdRule (nt, _, _, _) -> 
      Nt.equal_ci (Nt.unstub constructor) nt 
    ) ast in (
    match element with 
    | None -> Error (Format.asprintf "Dangling constructor identifier %a" Nt.pp (Nt.unstub constructor))
    | Some (TypeAnnotation (_, _, scs, _) as element) -> 
      handle_scs ast solver_ast constructor element scs 0
    | Some (A.ProdRule (_, _, rhss, _) as element) ->
      (* Find the matching production rule from ast, if one exists *)
      let rhs = List.find_mapi (fun i rhs -> match rhs with 
      | A.StubbedRhs _ -> None 
      | A.Rhs (ges, _, _, _) -> 
        if List.length ges != List.length children then None
        else 
          if List.for_all2 (fun child ge ->  
            match child, ge with 
            | _, A.StubbedNonterminal _ -> false 
            | SA.Node ((constructor, _, _), _), Nonterminal (nt, _, _, _, _) -> 
              Nt.equal_ci (Nt.unstub constructor) nt
            | _, _ -> true
          ) children ges 
          then Some (rhs, i) else None
      ) rhss in 
      if rhs = None then 
        Error (Format.asprintf "Could not find an associated production rule for constructor '%a'" Nt.pp constructor) 
      else 
        let scs, rhs_idx = match Option.get rhs with 
        | (StubbedRhs _, _) -> assert false 
        | (Rhs (_, scs, _, _), idx) -> scs, idx
        in 
        handle_scs ast solver_ast constructor element scs rhs_idx)
  | _ -> Ok ()

let check_solver_ast: Ast.ast -> SolverAst.solver_ast -> (unit, string) result 
= fun ast solver_ast -> 
  SA.pp_print_solver_ast Format.std_formatter solver_ast;
  let* _ = check_start_symbol ast solver_ast in 
  check_syntax_semantics ast solver_ast
