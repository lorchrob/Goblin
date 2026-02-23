let dpll ppf ctx ast = 
  (* Only applicable if we can "easily" split the problem. 
     TODO: Find middle ground where you can split the problem even if there are some overlaps. *)
  if not (MergeOverlappingConstraints.detect_overlapping_constraints ast) then (
    (* Step 1: Resolve ambiguities in constraints *)
    let ast = DetectAmbiguities.detect_ambiguities ctx ast in
    Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Step 2: Abstract away dependent terms in the grammar *)
    Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
    let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Step 3: Divide and conquer *)
    Utils.debug_print Format.pp_print_string ppf "\n\nDivide and conquer:\n";
    let asts = DivideAndConquer.split_ast ast in 
    match asts with | None -> None | Some asts -> 
    List.iter (fun ast -> Utils.debug_print Ast.pp_print_ast ppf ast; Utils.debug_print Lib.pp_print_newline ppf ()) asts;
    Utils.debug_print Lib.pp_print_newline ppf ();

    if not !Flags.only_parse then (
      (* Step 3: DPLL engine *)
      Utils.debug_print Format.pp_print_string ppf "\nStarting DPLL engine:\n";
      let solver_asts = Parallelism.parallel_map (Dpll.dpll ctx dep_map) asts in 
      let solver_asts = 
      if List.mem (SolverAst.VarLeaf "infeasible") solver_asts 
        then [SolverAst.VarLeaf "infeasible"]
        else solver_asts
      in
      Utils.debug_print Format.pp_print_string ppf "\nOutputs from DPLL engines:\n";
      List.iter (Utils.debug_print SolverAst.pp_print_solver_ast ppf) solver_asts;

      (* Step 4: Recombine to single AST *)
      Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
      let solver_ast = Recombine.recombine solver_asts in 
      Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;
      
      (* Step 5: Compute dependent terms *)
      Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
      let solver_ast = ComputeDeps.compute_deps dep_map ast solver_ast in  
      Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast; 

      Some solver_ast
    ) else Some (VarLeaf ""))

  else None
