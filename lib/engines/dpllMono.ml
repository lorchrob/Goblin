(* TODO
  *!! Fix multiple solutions mode w/ dependencies
*)

let dpll ppf ctx ast = 
  
  Utils.debug_print Format.pp_print_string ppf "\nLifting overlapping dependencies:\n";
  let ast = MergeOverlappingConstraints.lift_overlapping_dependencies ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 1: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities_dpll ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Abstract away dependent terms in the grammar *)
  Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  if not !Flags.only_parse then (
    (* Step 3: DPLL engine *)
    Utils.debug_print Format.pp_print_string ppf "\nStarting DPLL engine:\n";
    let solver_ast = Dpll.dpll ctx dep_map ast in 
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;
    
    (* Step 4: Compute dependent terms *)
    Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let solver_ast = ComputeDeps.compute_deps dep_map ast solver_ast in  
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast; 

    solver_ast
  ) else VarLeaf ""
