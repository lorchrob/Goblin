(* SygusMono is redundant. If SygusMono is possible (only top-level constraints), 
   then SygusMono is equivalent to SygusDac. *)

let sygus ppf ctx ast =
  (* Step 1: Merge overlapping constraints *)
  Utils.debug_print Format.pp_print_string ppf "\nMerge overlapping constraints:\n";
  let ast = MergeOverlappingConstraints.merge_overlapping_constraints ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Resolve ambiguities in constraints *)
  let ast = ResolveAmbiguities.resolve_ambiguities ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 3: Convert NTExprs to Match expressions *)
  let ast = Utils.recurse_until_fixpoint ast (=) (NtExprToMatch.convert_nt_exprs_to_matches ctx) in
  Utils.debug_print Format.pp_print_string ppf "\nDesugaring NTExprs complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 4: Abstract away dependent terms in the grammar *)
  Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 5: Divide and conquer *)
  Utils.debug_print Format.pp_print_string ppf "\n\nDivide and conquer:\n";
  let asts = DivideAndConquer.split_ast ast in 
  match asts with | None -> None | Some asts -> 
  List.iter (fun ast -> Utils.debug_print Ast.pp_print_ast ppf ast; Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();

  (* Step 6.1: Translate to SyGuS problems *)
  Utils.debug_print Format.pp_print_string ppf "\nSyGuS translation:\n";
  List.iter (fun ast -> Utils.debug_print Sygus.pp_print_ast ppf (ctx, dep_map, ast); Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();

  Format.pp_print_flush ppf ();

  if not !Flags.only_parse then (
    (* Step 6.2: Call sygus engine *)
    Utils.debug_print Format.pp_print_string ppf "Calling SyGuS:";
    Utils.debug_print Lib.pp_print_newline ppf ();
    let sygus_outputs = Parallelism.parallel_map  (Sygus.call_sygus ctx dep_map) asts in
    List.iter (Utils.debug_print Format.pp_print_string ppf) sygus_outputs;
    
    (* Step 7: Parse SyGuS output *)
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

    (* Step 8: Recombine to single AST *)
    Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
    let sygus_ast = Recombine.recombine sygus_asts in 
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    Format.pp_print_flush ppf ();

    (* Step 9: Compute dependencies *)
    Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let sygus_ast = 
      if not (List.mem (SygusAst.VarLeaf "infeasible") sygus_asts)
      then ComputeDeps.compute_deps dep_map ast sygus_ast 
      else SygusAst.VarLeaf "infeasible"
    in  
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    (* Step 10: Bit flip mutations for BitList terms *)
    Utils.debug_print Format.pp_print_string ppf "\nBit flip mutations:\n";
    let sygus_ast = BitFlips.flip_bits sygus_ast in 
    Utils.debug_print SygusAst.pp_print_sygus_ast ppf sygus_ast;

    Some sygus_ast
  ) else Some (VarLeaf "")