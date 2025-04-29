let main_pipeline filename = 
  let ppf = Format.std_formatter in
  let input_string = Utils.read_file filename in 

  (* Step 0: Parse user input *)
  Utils.debug_print Format.pp_print_string ppf "Lexing and parsing complete:\n";
  let ast = Parsing.parse input_string in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 
  Utils.debug_print Format.pp_print_string ppf "\nSyntactic checks complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nType checking complete:\n";

  (* Step 3: Merge overlapping constraints *)
  Utils.debug_print Format.pp_print_string ppf "\nMerge overlapping constraints:\n";
  let ast = MergeOverlappingConstraints.merge_overlapping_constraints ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 4: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 5: Convert NTExprs to Match expressions *)
  let ast = Utils.recurse_until_fixpoint ast (=) (NtExprToMatch.convert_nt_exprs_to_matches ctx) in
  Utils.debug_print Format.pp_print_string ppf "\nDesugaring NTExprs complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 6: Abstract away dependent terms in the grammar *)
  Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (if !Flags.selected_engine = Flags.DPLL then 
    let result = Dpll.dpll ctx ast in 
    print_endline result; 
    Utils.crash "Exiting early :D");

  (* Step 7: Divide and conquer *)
  Utils.debug_print Format.pp_print_string ppf "\n\nDivide and conquer:\n";
  let asts = DivideAndConquer.split_ast ast in 
  List.iter (fun ast -> Utils.debug_print Ast.pp_print_ast ppf ast; Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();

  (* Step 8.1: Translate to SyGuS problems *)
  Utils.debug_print Format.pp_print_string ppf "\nSyGuS translation:\n";
  List.iter (fun ast -> Utils.debug_print Sygus.pp_print_ast ppf (ctx, dep_map, ast); Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();

  Format.pp_print_flush ppf ();

  if not !Flags.only_parse then (
    (* Step 8.2: Call sygus engine *)
    Utils.debug_print Format.pp_print_string ppf "Calling SyGuS:";
    Utils.debug_print Lib.pp_print_newline ppf ();
    let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in
    List.iter (Utils.debug_print Format.pp_print_string ppf) sygus_outputs;
    
    (* Step 9: Parse SyGuS output *)
    Utils.debug_print Format.pp_print_string ppf "\nParsing SyGuS output:\n";
    let sygus_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    let sygus_asts = List.map Result.get_ok sygus_asts in
    let sygus_asts = 
      if List.mem (SygusAst.VarLeaf "infeasible") sygus_asts 
      then [SygusAst.VarLeaf "infeasible"]
      else sygus_asts
    in
    Utils.debug_print Format.pp_print_string ppf "\nSyGuS ASTs:\n";
    List.iter (Utils.debug_print SygusAst.pp_print_sygus_ast ppf) sygus_asts;

    Format.pp_print_flush ppf ();

    (* Step 10: Recombine to single AST *)
    Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
    let sygus_ast = Recombine.recombine sygus_asts in 
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    Format.pp_print_flush ppf ();

    (* Step 11: Compute dependencies *)
    Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let sygus_ast = 
      if not (List.mem (SygusAst.VarLeaf "infeasible") sygus_asts)
      then ComputeDeps.compute_deps dep_map ast sygus_ast 
      else SygusAst.VarLeaf "infeasible"
    in  
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    (* Step 12: Bit flip mutations for BitList terms *)
    Utils.debug_print Format.pp_print_string ppf "\nBit flip mutations:\n";
    let sygus_ast = BitFlips.flip_bits sygus_ast in 
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    (* Step 13: Serialize! *)
    Utils.debug_print Format.pp_print_string ppf "\nSerializing:\n";
    let output = Utils.capture_output SygusAst.serialize sygus_ast in 
    Format.pp_print_string ppf output; 
    output
  ) else ""

let rec collect_results results =
  match results with
  | [] -> Ok []
  | Ok v :: rest ->
      (match collect_results rest with
        | Ok vs -> Ok (v :: vs)
        | Error e -> Error e)
  | Error e :: _ -> Error e

let sygusGrammarToPacket ast = 
  (* let ast = sortAst input_grammar in *)

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in

  (* Step 3: Merge overlapping constraints *)
  (* let ast = MergeOverlappingConstraints.merge_overlapping_constraints ast in *)

  (* Step 4: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in

  (* Step 5: Convert NTExprs to Match expressions *)
  let ast = NtExprToMatch.convert_nt_exprs_to_matches ctx ast in

  (* Step 6: Abstract away dependent terms in the grammar *)
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 

  (* Step 7: Divide and conquer *)
  let asts = DivideAndConquer.split_ast ast in 

  if not !Flags.only_parse then (
    (* Step 8: Call sygus engine *)
    let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in

    (* Step 9: Parse SyGuS output. *)
    let sygus_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    match collect_results sygus_asts with
    | Error e -> Error e
    | Ok sygus_asts -> 
      (* Catch infeasible response *)
      let sygus_asts = 
        if List.mem (SygusAst.VarLeaf "infeasible") sygus_asts 
        then [SygusAst.VarLeaf "infeasible"]
        else sygus_asts
      in

      (* Step 10: Recombine to single AST *)
      let sygus_ast = Recombine.recombine sygus_asts in 

      (* Step 11: Compute dependencies *)
      let sygus_ast = 
        if not (List.mem (SygusAst.VarLeaf "infeasible") sygus_asts)
        then ComputeDeps.compute_deps dep_map ast sygus_ast 
        else SygusAst.VarLeaf "infeasible"
      in  

      (* Step 12: Bit flip mutations *)
      let sygus_ast = BitFlips.flip_bits sygus_ast in

      (* Step 13: Serialize! *)
      let output = SygusAst.serialize_bytes SygusAst.Big sygus_ast in 
      Ok output) 
    else 
      let dummy_output = Bytes.empty, Bytes.empty in
      Ok dummy_output
