let dpll ppf ctx ast = 
  (* Only applicable if we can "easily" split the problem. 
     TODO: Find middle ground where you can split the problem even if there are some overlaps. *)
  if not (MergeOverlappingConstraints.detect_overlapping_constraints ast) then (
    (* Step 4: Resolve ambiguities in constraints *)
    let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
    Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Step 5: Abstract away dependent terms in the grammar *)
    Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
    let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
    Utils.debug_print Ast.pp_print_ast ppf ast;

    if not !Flags.only_parse then (
      Utils.debug_print Format.pp_print_string ppf "\nStarting DPLL engine:\n";
      let sygus_ast = Dpll.dpll ctx ast in 
      Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;
      
      Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
      let sygus_ast = ComputeDeps.compute_deps dep_map ast sygus_ast in  
      Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast; 

      Utils.debug_print Format.pp_print_string ppf "\nSerializing:\n";
      let output = Utils.capture_output SygusAst.serialize sygus_ast in 
      Format.pp_print_string ppf output; 
      Some (sygus_ast, output)
    ) else Some (VarLeaf "", "")) 

  else None