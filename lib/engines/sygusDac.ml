(* SygusMono is redundant. If SygusMono is possible (only top-level constraints), 
   then SygusMono is equivalent to SygusDac. *)

let sygus ppf ctx ast =
  (*!! Right now, this check is strict enough to rule out the grammars from SAECRED.
       Maybe the check can determine if there are any semantic constraints on list-type recursion *)
  match SyntaxChecker.check_if_recursive ast with | true -> None | false -> 

  (* Step 1: Merge overlapping constraints *)
  Utils.debug_print Format.pp_print_string ppf "\nMerge overlapping constraints:\n";
  let ast = Utils.recurse_until_fixpoint ast (=) MergeOverlappingConstraints.merge_overlapping_constraints in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Step 2: Resolve ambiguities in constraints *)
  let ast = DetectAmbiguities.detect_ambiguities ctx ast in
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
  List.iter (fun ast -> Utils.debug_print SmtPrinter.pp_print_ast ppf (ctx, dep_map, ast); Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();

  Format.pp_print_flush ppf ();

  if not !Flags.only_parse then (
    (* Step 6.2: Call sygus engine *)
    Utils.debug_print Format.pp_print_string ppf "Calling SyGuS:";
    Utils.debug_print Lib.pp_print_newline ppf ();
    let sygus_outputs = Parallelism.parallel_map  (SmtPrinter.call_sygus ctx dep_map) asts in
    List.iter (Utils.debug_print Format.pp_print_string ppf) sygus_outputs;
    
    (* Step 7: Parse SyGuS output *)
    Utils.debug_print Format.pp_print_string ppf "\nParsing SyGuS output:\n";
    let solver_asts = List.map2 Parsing.parse_sygus sygus_outputs asts in
    let solver_asts = List.map Result.get_ok solver_asts in
    let solver_asts = 
      if List.mem (SolverAst.VarLeaf "infeasible") solver_asts 
      then [SolverAst.VarLeaf "infeasible"]
      else solver_asts
    in
    Utils.debug_print Format.pp_print_string ppf "\nSolver ASTs:\n";
    List.iter (Utils.debug_print SolverAst.pp_print_solver_ast ppf) solver_asts;

    Format.pp_print_flush ppf ();

    (* Step 8: Recombine to single AST *)
    Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
    let solver_ast = Recombine.recombine solver_asts in 
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

    Format.pp_print_flush ppf ();

    (* Step 9: Compute dependencies *)
    Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
    let solver_ast = 
      if not (List.mem (SolverAst.VarLeaf "infeasible") solver_asts)
      then ComputeDeps.compute_deps dep_map ast solver_ast 
      else SolverAst.VarLeaf "infeasible"
    in  
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

    (* Step 10: Bit flip mutations for BitList terms *)
    Utils.debug_print Format.pp_print_string ppf "\nBit flip mutations:\n";
    let solver_ast = BitFlips.flip_bits solver_ast in 
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

    Some solver_ast
  ) else Some (VarLeaf "")
