let dpll_leaf ppf ctx ast = 
  (* Resolve ambiguities in constraints *)
  let ast = DetectAmbiguities.detect_ambiguities ctx ast in
  Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Abstract away dependent terms in the grammar *)
  Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
  let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
  Utils.debug_print Ast.pp_print_ast ppf ast;

  if not !Flags.only_parse then (
    (* DPLL engine *)
    Utils.debug_print Format.pp_print_string ppf "\nStarting DPLL engine:\n";
    let solver_ast = Dpll.dpll ctx ast in 
    Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;
    
    dep_map, solver_ast
  ) else Utils.StringMap.empty, VarLeaf ""


let sygus_leaf: Format.formatter -> TypeChecker.context -> Ast.ast -> Ast.semantic_constraint Utils.StringMap.t * SolverAst.solver_ast
= fun ppf ctx ast ->
  if not !Flags.only_parse then (
    (* Resolve ambiguities in constraints *)
    let ast = DetectAmbiguities.detect_ambiguities ctx ast in
    Utils.debug_print Format.pp_print_string ppf "\nResolving grammar ambiguities complete:\n";
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Abstract away dependent terms in the grammar *)
    Utils.debug_print Format.pp_print_string ppf "\nDependent term abstraction:\n";
    let dep_map, ast, ctx = AbstractDeps.abstract_dependencies ctx ast in 
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Convert NTExprs to Match expressions *)
    let ast = Utils.recurse_until_fixpoint ast (=) (NtExprToMatch.convert_nt_exprs_to_matches ctx) in
    Utils.debug_print Format.pp_print_string ppf "\nDesugaring NTExprs complete:\n";
    Utils.debug_print Ast.pp_print_ast ppf ast;

    (* Call sygus engine *)
    Utils.debug_print Format.pp_print_string ppf "Calling SyGuS:";
    Utils.debug_print Lib.pp_print_newline ppf ();
    let sygus_output = SmtPrinter.call_sygus ctx dep_map ast in
    Utils.debug_print Format.pp_print_string ppf sygus_output;
    
    (* Parse SyGuS output *)
    Utils.debug_print Format.pp_print_string ppf "\nParsing SyGuS output:\n";
    let solver_ast = Parsing.parse_sygus sygus_output ast in
    let solver_ast = Result.get_ok solver_ast in
    Utils.debug_print Format.pp_print_string ppf "\nSOLVER ASTs:\n";
    Format.pp_print_flush ppf ();

    dep_map, solver_ast
  ) else Utils.StringMap.empty, VarLeaf ""

let dac ppf ctx ast =
  match SyntaxChecker.check_if_recursive ast with | true -> None | false -> 
    
  (* Merge overlapping constraints *)
  Utils.debug_print Format.pp_print_string ppf "\nMerge overlapping constraints:\n";
  let ast = Utils.recurse_until_fixpoint ast (=) MergeOverlappingConstraints.merge_overlapping_constraints in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Divide and conquer *)
  Utils.debug_print Format.pp_print_string ppf "\n\nDivide and conquer:\n";
  let asts = DivideAndConquer.split_ast ast in 
  match asts with | None -> None | Some asts -> List.iter (fun ast -> Utils.debug_print Ast.pp_print_ast ppf ast; Utils.debug_print Lib.pp_print_newline ppf ()) asts;
  Utils.debug_print Lib.pp_print_newline ppf ();
   
  (* Race leaf-level solvers *)
  let dep_maps, solver_asts = Parallelism.parallel_map  (fun ast -> 
    Parallelism.race_n [
      (fun () -> sygus_leaf ppf ctx ast), "sygus_leaf" ;
      (fun () -> dpll_leaf ppf ctx ast), "dpll_leaf" ;
    ]
  ) asts |> List.split in

  let dep_map = List.fold_left (Utils.StringMap.merge Lib.union_keys) Utils.StringMap.empty dep_maps in 

  (* Recombine to single AST *)
  Utils.debug_print Format.pp_print_string ppf "\nRecombining to single AST:\n";
  let solver_ast = Recombine.recombine solver_asts in 
  Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

  Format.pp_print_flush ppf ();

  (* Compute dependencies *)
  Utils.debug_print Format.pp_print_string ppf "\nComputing dependencies:\n";
  let solver_ast = 
    if not (List.mem (SolverAst.VarLeaf "infeasible") solver_asts)
    then ComputeDeps.compute_deps dep_map ast solver_ast 
    else SolverAst.VarLeaf "infeasible"
  in  
  Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

  (* Bit flip mutations for BitList terms *)
  Utils.debug_print Format.pp_print_string ppf "\nBit flip mutations:\n";
  let solver_ast = BitFlips.flip_bits solver_ast in 
  Utils.debug_print SolverAst.pp_print_solver_ast ppf solver_ast;

  Some solver_ast
