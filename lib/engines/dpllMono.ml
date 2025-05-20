let dpll ppf ctx ast =  
  (* Step 1: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Abstract away dependent terms in the grammar *)
  Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  if not !Flags.only_parse then (
    (* Step 3: DPLL engine *)
    Utils.debug_print Format.pp_print_string ppf "\nStarting DPLL engine:\n";
    let sygus_ast = Dpll.dpll ctx ast in 
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;
    
    (* Step 4: Compute dependent terms *)
    Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let sygus_ast = ComputeDeps.compute_deps dep_map ast sygus_ast in  
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast; 

    sygus_ast
  ) else VarLeaf ""