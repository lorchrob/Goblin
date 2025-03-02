(* input string -> output serialized packet *)
let main_pipeline input_string = 
  let ppf = Format.std_formatter in

  (* Step 0: Parse user input *)
  Debug.debug_print Format.pp_print_string ppf "Lexing and parsing complete:\n";
  let ast = Parsing.parse input_string in 
  Debug.debug_print Ast.pp_print_ast ppf ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 
  Debug.debug_print Format.pp_print_string ppf "\nSyntactic checks complete:\n";
  Debug.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in
  Debug.debug_print Format.pp_print_string ppf "\nType checking complete:\n";

  (* Step 3: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
  Debug.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Debug.debug_print Ast.pp_print_ast ppf ast;

  (* Step 4: Convert NTExprs to Match expressions *)
  let ast = Utils.recurse_until_fixpoint ast (=) (NtExprToMatch.convert_nt_exprs_to_matches ctx) in
  Debug.debug_print Format.pp_print_string ppf "\nDesugaring NTExprs complete:\n";
  Debug.debug_print Ast.pp_print_ast ppf ast;

  (* Step 5: Abstract away dependent terms in the grammar *)
  Debug.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Debug.debug_print Ast.pp_print_ast ppf ast;

  (* Step 6: Divide and conquer *)
  Debug.debug_print Format.pp_print_string ppf "\n\nDivide and conquer:\n";
  let asts = DivideAndConquer.split_ast ast in 
  List.iter (fun ast -> Debug.debug_print Ast.pp_print_ast ppf ast; Debug.debug_print Lib.pp_print_newline ppf ()) asts;
  Debug.debug_print Lib.pp_print_newline ppf ();

  (* Step 7.1: Translate to SyGuS problems *)
  Debug.debug_print Format.pp_print_string ppf "\nSyGuS translation:\n";
  List.iter (fun ast -> Debug.debug_print Sygus.pp_print_ast ppf (ctx, dep_map, ast); Debug.debug_print Lib.pp_print_newline ppf ()) asts;
  Debug.debug_print Lib.pp_print_newline ppf ();

  Format.pp_print_flush Format.std_formatter ();

  if not !Debug.only_parse then (
    (* Step 7.2: Call sygus engine *)
    Debug.debug_print Format.pp_print_string ppf "Calling SyGuS:";
    Debug.debug_print Lib.pp_print_newline ppf ();
    let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in
    List.iter (Debug.debug_print Format.pp_print_string ppf) sygus_outputs;
    
    (* Step 8: Parse SyGuS output *)
    Debug.debug_print Format.pp_print_string ppf "\nParsing SyGuS output:\n";
    let sygus_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    let sygus_asts = List.map Result.get_ok sygus_asts in
    let sygus_asts = 
      if List.mem (SygusAst.VarLeaf "infeasible") sygus_asts 
      then [SygusAst.VarLeaf "infeasible"]
      else sygus_asts
    in
    Debug.debug_print Format.pp_print_string ppf "\nSyGuS ASTs:\n";
    List.iter (Debug.debug_print SygusAst.pp_print_sygus_ast ppf) sygus_asts;

    Format.pp_print_flush Format.std_formatter ();

    (* Step 9: Recombine to single AST *)
    Debug.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
    let sygus_ast = Recombine.recombine sygus_asts in 
    Debug.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    Format.pp_print_flush Format.std_formatter ();

    (* Step 10: Compute dependencies *)
    Debug.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let sygus_ast = 
      if not (List.mem (SygusAst.VarLeaf "infeasible") sygus_asts)
      then ComputeDeps.compute_deps dep_map ast sygus_ast 
      else SygusAst.VarLeaf "infeasible"
    in  
    Debug.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    (* Step 11: Bit flip mutations for BitList terms *)
    Debug.debug_print Format.pp_print_string ppf "\nBit flip mutations:\n";
    let sygus_ast = BitFlips.flip_bits sygus_ast in 
    Debug.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    (* Step 12: Serialize! *)
    Debug.debug_print Format.pp_print_string ppf "\nSerializing:\n";
    let output = Utils.capture_output SygusAst.serialize sygus_ast in 
    Format.pp_print_string ppf output; 
    Lib.pp_print_newline ppf (); 
    output
  ) else "dummy output"

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

  (* Step 3: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in

  (* Step 4: Convert NTExprs to Match expressions *)
  let ast = NtExprToMatch.convert_nt_exprs_to_matches ctx ast in

  (* Step 5: Abstract away dependent terms in the grammar *)
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 

  (* Step 6: Divide and conquer *)
  let asts = DivideAndConquer.split_ast ast in 

  if not !Debug.only_parse then (
    (* Step 7: Call sygus engine *)
    let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in

    (*!! TODO: Update to handle infeasible case as above *)
    (* Step 8: Parse SyGuS output. *)
    let sygus_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    match collect_results sygus_asts with
    | Error e -> Error e
    | Ok sygus_asts -> 
      (* Step 9: Recombine to single AST *)
      let sygus_ast = Recombine.recombine sygus_asts in 

      (* Step 10: Compute dependencies *)
      let sygus_ast = ComputeDeps.compute_deps dep_map ast sygus_ast in 

      (* Step 11: Bit flip mutations *)
      let sygus_ast = BitFlips.flip_bits sygus_ast in

      (* Step 12: Serialize! *)
      let output = SygusAst.serialize_bytes SygusAst.Big sygus_ast in 
      Ok output) 
    else 
      let dummy_output = Bytes.empty, Bytes.empty in
      Ok dummy_output