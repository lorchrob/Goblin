(* 
     TODO: Give positions in error messages 
     * Optimization: infer tighter set-logic 
     TODO: Clean up crud in output when Utils.crash is called 
     TODO: examples.md in user doc
     
     FUTURE: Flag that allows you to disable engines in race mode (useful if one engine is buggy on a class of problems)
     FUTURE: More fine-grained check if divide and conquer engines are usable. Right now,
           rejecting all recursive grammars. But really this only is necessary for sygus_dac.
     FUTURE: I think we could support arbitrary recursive functions in the dpll engines (at least, dpll_mono) 
             by simply unrolling the function definition as far as you need on the fly 
     FUTURE: Something akin to inherited attributes 
     FUTURE: Structural constraints 
     FUTURE: Quantifiers in the DSL 
     FUTURE: Finite model finding engine -- synthesize recursive functions to capture the constraints, 
             use define-fun-rec, and let the solver do the unfolding
     FUTURE: Paper Kartik sent about using catamorphisms (generalized folds) for ADT decision procedures. 
             We should always be able to define folds for the ADTs we have since they have well-founded recursion 
             and no higher-order function types (leaf-level nonterminals can't have function types)
     FUTURE: We must be able to handle dot notation constraints better by introducing new variables 
             and constraint passing
     FUTURE: Sygus forward declarations
     FUTURE: Revisit divide and conquer
     FUTURE: Surface level language w/ support for attributes
*)

(* TODO for nice surface-level language
    
    * Means of passing variables around CLP-style 
    * disambiguating an NT reference with <nt>[i]

*)

let main_pipeline ?(engine: Flags.engine option = None) ?(grammar: Ast.ast option) filename = 
  Printexc.record_backtrace true;
  let ppf = Format.std_formatter in

  let ast = match grammar with 
  | Some ast -> ast 
  | None -> 
    let input_string = Utils.read_file filename in 

    (* Parse user input *)
    Utils.debug_print Format.pp_print_string ppf "Lexing and parsing complete:\n";
    let ast = Parsing.parse input_string in 
    Utils.debug_print Ast.pp_print_ast ppf ast;
    ast
  in

  (* Desugar type annotation constraints *) 
  let ast = EliminateTaConstraints.eliminate_ta_constraints ast in 
  Utils.debug_print Format.pp_print_string ppf "\nType annotation constraints eliminated:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 
  Utils.debug_print Format.pp_print_string ppf "\nSyntactic checks complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nType checking complete:\n";

  (* Attribute checking *)
  let ast = AttributeChecker.check_attributes ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nAttribute checking complete:\n";

  (* Desugar attributes *)
  Utils.debug_print Format.pp_print_string ppf "\nDesugaring attributes:\n";
  let ast = DesugarAttributes.desugar_attributes ctx ast in
  (*!! Ideally, the checker would take as input the base AST, not the desugared one. *)
  let ast_to_return = ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Populate nonterminal indices *)
  Utils.debug_print Format.pp_print_string ppf "\nPopulating indices:\n";
  let ast = PopulateIndices.populate_indices ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Run engine(s) *)
  let solver_ast = 
    match engine, !Flags.selected_engine with 
    (* Single engine mode.
       Two means of selecting engines -- command-line arg (default for users), 
       or passing a functional argument (for testing).
       Function arg trumps command-line arg. *)
    | Some DpllMono, _ -> DpllMono.dpll ppf ctx ast
    | Some DpllDac, _  -> 
      (match DpllDac.dpll ppf ctx ast with
      | Some result -> result 
      | None -> Utils.error_no_pos "dpll_dac engine not applicable to this input")
    | Some SygusDac, _ -> 
      (match SygusDac.sygus ppf ctx ast with  
      | Some result -> result 
      | None -> Utils.error_no_pos "sygus_dac engine not applicable to this input")
    | Some MixedDac, _ -> 
      (match MixedDac.dac ppf ctx ast with
      | Some result -> result 
      | None -> Utils.error_no_pos "mixed_dac engine not applicable to this input")
    (* Race mode *)
    | Some Race, _ -> (
      try 
        Parallelism.race_n_opt [
          (fun () -> DpllDac.dpll ppf ctx ast), "dpll_dac" ;
          (fun () -> Some (DpllMono.dpll ppf ctx ast)), "dpll_mono" ;
          (fun () -> (SygusDac.sygus ppf ctx ast)), "sygus_dac" ;
          (fun () -> (MixedDac.dac ppf ctx ast)), "mixed_dac" ;
        ]
      with Parallelism.AllReturnedNone -> 
        Utils.crash "No engine produced a result"
      )
    | _, DpllMono -> DpllMono.dpll ppf ctx ast
    | _, DpllDac -> 
      (match DpllDac.dpll ppf ctx ast with
      | Some result -> result 
      | None -> Utils.error_no_pos "dpll_dac engine not applicable to this input")
    | _, SygusDac -> 
      (match SygusDac.sygus ppf ctx ast with  
      | Some result -> result 
      | None -> Utils.error_no_pos "sygus_dac engine not applicable to this input")
    | _, MixedDac -> 
      (match MixedDac.dac ppf ctx ast with
      | Some result -> result 
      | None -> Utils.error_no_pos "mixed_dac engine not applicable to this input")
    (* Race mode *)
    | _, Race -> 
      try 
        Parallelism.race_n_opt [
          (fun () -> DpllDac.dpll ppf ctx ast), "dpll_dac" ;
          (fun () -> Some (DpllMono.dpll ppf ctx ast)), "dpll_mono" ;
          (fun () -> (SygusDac.sygus ppf ctx ast)), "sygus_dac" ;
          (fun () -> (MixedDac.dac ppf ctx ast)), "mixed_dac" ;
        ]
      with Parallelism.AllReturnedNone -> 
        Utils.crash "No engine produced a result"

  in

  (* Serialize! *)
  Utils.debug_print Format.pp_print_string ppf "\nFinal result:\n";
  let output = Utils.capture_output Serialize.serialize solver_ast in 
  if not !Flags.multiple_solutions then (
    if !Flags.output_format = Flags.SExpression then 
      SolverAst.pp_print_solver_ast Format.std_formatter solver_ast
    else if !Flags.output_format = Flags.Hex then 
      let ast_bytes, _ = Serialize.serialize_bytes Big [] solver_ast in
      Utils.print_bytes_as_hex ast_bytes 
    else if !Flags.output_format = Flags.HexPacked then 
      let ast_bytes = Serialize.serialize_bytes_packed solver_ast in
      Utils.print_bytes_as_hex ast_bytes 
  );
  solver_ast, output, ast_to_return

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

  (* Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 

  (* Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in

  let ast = PopulateIndices.populate_indices ast in

  (* Merge overlapping constraints *)
  (* let ast = MergeOverlappingConstraints.merge_overlapping_constraints ast in *)

  (* Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in

  (* Convert NTExprs to Match expressions *)
  let ast = NtExprToMatch.convert_nt_exprs_to_matches ctx ast in

  (* Abstract away dependent terms in the grammar *)
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 

  (* Divide and conquer *)
  let asts = DivideAndConquer.split_ast ast |> Option.get in 

  if not !Flags.only_parse then (
    (* Call sygus engine *)
    let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in

    (* Parse SyGuS output. *)
    let solver_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    match collect_results solver_asts with
    | Error e -> Error e
    | Ok solver_asts -> 
      (* Catch infeasible response *)
      let solver_asts = 
        if List.mem (SolverAst.VarLeaf "infeasible") solver_asts 
        then [SolverAst.VarLeaf "infeasible"]
        else solver_asts
      in

      (* Recombine to single AST *)
      let solver_ast = Recombine.recombine solver_asts in 

      (* Compute dependencies *)
      let solver_ast = 
        if not (List.mem (SolverAst.VarLeaf "infeasible") solver_asts)
        then ComputeDeps.compute_deps dep_map ast solver_ast 
        else SolverAst.VarLeaf "infeasible"
      in  

      (* Bit flip mutations *)
      let solver_ast = BitFlips.flip_bits solver_ast in

      (* Serialize! *)
      let output = Serialize.serialize_bytes Serialize.Big [] solver_ast in 
      Ok output) 
    else 
      let dummy_output = Bytes.empty, Bytes.empty in
      Ok dummy_output
