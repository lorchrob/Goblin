let main_pipeline input_string = 
  let ppf = Format.std_formatter in

  (* Step 0: Parse user input *)
  Format.fprintf ppf "Lexing and parsing:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let ast = Utils.parse input_string in 
  Ast.pp_print_ast ppf ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Syntactic checks complete";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Type checking complete";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();

  (* Step 3: Abstract away dependent terms in the grammar *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Dependent term abstraction:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Ast.pp_print_ast ppf ast;
  Lib.pp_print_newline ppf;

  (* Step 4: Divide and conquer *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Divide and conquer:";
  Lib.pp_print_newline ppf;
  let asts = DivideAndConquer.split_ast ast in 
  List.iter (fun ast -> Ast.pp_print_ast ppf ast; Lib.pp_print_newline ppf) asts;
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();

  (* Step 5: Prune grammars (both within grammars, and unreachable stubs) *)
  (* TODO, if needed for better performance *)

  (* Step 6: Translate to SyGuS problems *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "SyGuS translation:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  List.iter (fun ast -> Sygus.pp_print_ast ppf ctx dep_map ast; Lib.pp_print_newline ppf) asts;
  Lib.pp_print_newline ppf;
  
  (* Step 7: Call sygus engine *)
  Format.fprintf ppf "Calling SyGuS:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in
  List.iter (Format.pp_print_string ppf) sygus_outputs;

  (* Step 8: Parse SyGuS output *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Parsing SyGuS output:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_asts = List.map2 Utils.parse_sygus sygus_outputs asts in
  Format.fprintf ppf "SyGuS ASTs:\n";
  List.iter (SygusAst.pp_print_sygus_ast ppf) sygus_asts;
  

  (* Step 9: Recombine to single AST *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Recombining to single AST:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_ast = Recombine.recombine sygus_asts in 
  SygusAst.pp_print_sygus_ast ppf sygus_ast;

  (* Step 10: Compute dependencies *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Computing dependencies:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_ast = ComputeDeps.compute_deps dep_map sygus_ast in 
  SygusAst.pp_print_sygus_ast ppf sygus_ast;

  (* Step 11: Serialize! *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Serializing:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let output = Utils.capture_output SygusAst.serialize sygus_ast in 
  Format.pp_print_string ppf output; 
  Lib.pp_print_newline ppf; 
  output