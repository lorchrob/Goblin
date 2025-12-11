module SA = SolverAst
module A = Ast
module R = Res 

let (let*) = Res.(>>=)

(*!!
  * currently assumes that the leaf nodes are labeled (not the case in sygus_dac),
    for now it's not too bad but could miss falsified type annotation semantic constraints 
*)

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
| A.ProdRule (nt, _, _, _) :: _, SA.Node ((constructor, _), _) -> 
  if Utils.str_eq_ci nt (Utils.extract_base_name constructor) 
    then Ok () 
  else 
  Error ("Solver AST root constructor '" ^ constructor ^ "' does not match the AST start symbol '" ^ nt ^ "'")
| A.TypeAnnotation _ :: _, _ -> Utils.crash "Unexpected case in check_start_symbol"
| _ -> Error "Solver AST root node is a leaf node"

let rec is_nt_applicable: SolverAst.solver_ast -> (string * int option) list -> bool 
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

let handle_scs ast solver_ast constructor element scs = 
  let scs = List.map (fun sc -> match sc with 
  | A.SmtConstraint (expr, p) -> 
    if is_sc_applicable expr solver_ast || (* type annotation constraints are always applicable *)
       match element with | A.TypeAnnotation _ -> true | A.ProdRule _ -> false
    then (
      (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is applicable in %a"
        A.pp_print_expr expr
        SA.pp_print_solver_ast solver_ast
        );
      ComputeDeps.evaluate false solver_ast ast element expr)
    else (
      (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is not applicable in %a"
        A.pp_print_expr expr
        SA.pp_print_solver_ast solver_ast
        );
      [BConst (true, p)]) (* If sc is not applicable, it trivially holds *)
  | DerivedField (nt, expr, p) -> 
    let expr = A.CompOp (NTExpr ([], [nt, Some 0], p), Eq, expr, p) in
    (if !Flags.debug then Format.fprintf Format.std_formatter "Constraint %a is applicable in %a"
      A.pp_print_expr expr
      SA.pp_print_solver_ast solver_ast
      );
    ComputeDeps.evaluate false solver_ast ast element expr
  | AttrDef _ -> assert false
  ) scs in
  let b = List.exists (fun sc -> match sc with 
  | [A.BConst (false, _)] -> true 
  | [BConst (true, _)] -> false 
  | _ -> Utils.crash "Unexpected pattern in check_syntax_semantics"
  ) scs in
  if b then Error ("Semantic constraint on constructor '" ^ constructor ^ "' is falsified") else
  Ok ()

let rec check_syntax_semantics: Ast.ast -> SolverAst.solver_ast -> (unit, string) result 
= fun ast solver_ast -> match solver_ast with 
  | Node ((constructor, _), children) -> 
    (* In dpll divide and conquer module, 
       we get an extra nesting of stub and concrete NTs 
       for some reason. *)
    let skip_condition = 
      match children with 
      | [Node ((constructor2, _), _)] ->
        Utils.str_eq_ci constructor (Utils.extract_base_name constructor2)
      | _ -> false
    in
    if skip_condition then check_syntax_semantics ast (List.hd children) else
      
    let* _ = R.seq (List.map (check_syntax_semantics ast) children) in
    (* Find this node's corresponding AST element *) 
    let element = List.find_opt (fun element -> match element with
    | A.TypeAnnotation (nt, _, _, _) -> 
      Utils.str_eq_ci (Utils.extract_base_name constructor) nt 
    | A.ProdRule (nt, _, _, _) -> 
      Utils.str_eq_ci (Utils.extract_base_name constructor) nt 
    ) ast in (
    match element with 
    | None -> Error ("Dangling constructor identifier " ^ (Utils.extract_base_name constructor))
    | Some (TypeAnnotation (_, _, scs, _) as element) -> 
      handle_scs ast solver_ast constructor element scs
    | Some (A.ProdRule (_, _, rhss, _) as element) ->
      (* Find the matching production rule from ast, if one exists *)
      let rhs = List.find_opt (fun rhs -> match rhs with 
      | A.StubbedRhs _ -> false 
      | A.Rhs (ges, _, _, _) -> 
        if List.length ges != List.length children then false 
        else 
          List.for_all2 (fun child ge ->  
            match child, ge with 
            | _, A.StubbedNonterminal _ -> false 
            | SA.Node ((constructor, _), _), Nonterminal (nt, _, _, _) -> 
              Utils.str_eq_ci (Utils.extract_base_name constructor) nt
            | _, _ -> true
          ) children ges
      ) rhss in 
      if rhs = None then 
        Error ("Could not find an associated production rule for constructor '" ^ constructor ^"'") 
      else 
        let scs = match Option.get rhs with 
        | (StubbedRhs _) -> assert false 
        | (Rhs (_, scs, _, _)) -> scs 
        in 
        handle_scs ast solver_ast constructor element scs)
  | _ -> Ok ()

let check_solver_ast: Ast.ast -> SolverAst.solver_ast -> (unit, string) result 
= fun ast solver_ast -> 
  SA.pp_print_solver_ast Format.std_formatter solver_ast;
  let* _ = check_start_symbol ast solver_ast in 
  check_syntax_semantics ast solver_ast
