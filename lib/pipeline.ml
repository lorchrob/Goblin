(*!! TODO: Race engines 
        then engine flag should refer to set of enabled engines
        have to support optional return types if the engine is not always applicable
     TODO: Parallelize divide and conquer (DAC) engines. 
     TODO: divide-and-conquer where you race sygus and dpll on leaf problems
     TODO: Support dependency computation w/ dot notation
     TODO: Support dependency overlapping with sygus expr 
     TODO: Experimental evaluation
     TODO: Write paper *)

let main_pipeline filename = 
  Printexc.record_backtrace true;

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

  (* Step 3: Run engine(s) *)
  let sygus_ast = 
    match !Flags.selected_engine with 
    (* Single engine mode *)
    | Some DpllMono -> DpllMono.dpll ppf ctx ast
    | Some DpllDac -> DpllDac.dpll ppf ctx ast |> Option.get
    | Some SygusDac -> SygusDac.sygus ppf ctx ast
    (* Race mode *)
    | None -> 
      Parallelism.race_n [
        (fun () -> Option.get (DpllDac.dpll ppf ctx ast)), "dpll_dac" ;
        (fun () -> DpllMono.dpll ppf ctx ast), "dpll_mono" ;
        (fun () -> SygusDac.sygus ppf ctx ast), "sygus_dac" ;
      ]
  in

  (* Step 3: Serialize! *)
  Utils.debug_print Format.pp_print_string ppf "\nSerializing:\n";
  let output = Utils.capture_output SygusAst.serialize sygus_ast in 
  Format.pp_print_string ppf output; 
  sygus_ast, output

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
