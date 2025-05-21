let dpll ppf ctx ast = 
  (* Only applicable if we can "easily" split the problem. 
     TODO: Find middle ground where you can split the problem even if there are some overlaps. *)
  if not (MergeOverlappingConstraints.detect_overlapping_constraints ast) then (
    (* Step 1: Resolve ambiguities in constraints *)
    let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
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
      let sygus_asts = Parallelism.parallel_map (Dpll.dpll ctx) asts in 
      let sygus_asts = 
      if List.mem (SygusAst.VarLeaf "infeasible") sygus_asts 
        then [SygusAst.VarLeaf "infeasible"]
        else sygus_asts
      in
      Utils.debug_print Format.pp_print_string ppf "\nOutputs from DPLL engines:\n";
      List.iter (Utils.debug_print SygusAst.pp_print_sygus_ast ppf) sygus_asts;

      (* Step 4: Recombine to single AST *)
      Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
      let sygus_ast = Recombine.recombine sygus_asts in 
      Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;
      
      (* Step 5: Compute dependent terms *)
      Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
      let sygus_ast = ComputeDeps.compute_deps dep_map ast sygus_ast in  
      Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast; 

      Some sygus_ast
    ) else Some (VarLeaf ""))

  else None