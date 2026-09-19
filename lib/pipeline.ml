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

(* Everything up to but not including the search. Returns the AST the engines
   consume, the typing context, and the AST the checker consumes. *)
let front_end ?(grammar: Ast.ast option) filename =
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

  (* Must precede syntax and type checking, which need each inherited attribute's owner *)
  let ast = ScopeInhAttrs.scope_inh_attrs ast in 
  Utils.debug_print Format.pp_print_string ppf "\nInherited attributes scoped:\n";
  Utils.debug_print Ast.pp_print_ast ppf ast;

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

  (* Indices resolved but attributes still present, so the checker validates the
     user's grammar rather than Goblin's desugaring of it (todo-evaluator.md, D1) *)
  let base_ast = ResolveAmbiguities.resolve_ambiguities ctx (PopulateIndices.populate_indices ast) in

  (* Desugar attributes *)
  Utils.debug_print Format.pp_print_string ppf "\nDesugaring attributes:\n";
  let ast = DesugarAttributes.desugar_attributes ctx ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  (* Populate nonterminal indices *)
  Utils.debug_print Format.pp_print_string ppf "\nPopulating indices:\n";
  let ast = PopulateIndices.populate_indices ast in
  Utils.debug_print Ast.pp_print_ast ppf ast;

  ast, ctx, base_ast

let main_pipeline ?(engine: Flags.engine option = None) ?(grammar: Ast.ast option) filename =
  let ppf = Format.std_formatter in
  let ast, ctx, base_ast = front_end ?grammar filename in

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
    (* Race mode *)
    | Some Race, _ -> (
      try 
        Parallelism.race_n_opt [
          (fun () -> DpllDac.dpll ppf ctx ast), "dpll_dac" ;
          (fun () -> Some (DpllMono.dpll ppf ctx ast)), "dpll_mono" ;
        ]
      with Parallelism.AllReturnedNone -> 
        Utils.crash "No engine produced a result"
      )
    | _, DpllMono -> DpllMono.dpll ppf ctx ast
    | _, DpllDac -> 
      (match DpllDac.dpll ppf ctx ast with
      | Some result -> result 
      | None -> Utils.error_no_pos "dpll_dac engine not applicable to this input")
    (* Race mode *)
    | _, Race -> 
      try 
        Parallelism.race_n_opt [
          (fun () -> DpllDac.dpll ppf ctx ast), "dpll_dac" ;
          (fun () -> Some (DpllMono.dpll ppf ctx ast)), "dpll_mono" ;
        ]
      with Parallelism.AllReturnedNone -> 
        Utils.crash "No engine produced a result"

  in

  (* Serialize! *)
  Utils.debug_print Format.pp_print_string ppf "\nFinal result:\n";
  let output = Utils.capture_output Serialize.serialize solver_ast in 
  if not !Flags.multiple_solutions then (
    match !Flags.output_format with
    | Flags.SExpression -> SolverAst.pp_print_solver_ast Format.std_formatter solver_ast
    | Flags.Hex -> Serialize.print_hex (Serialize.serialize_bytes Big []) solver_ast
    | Flags.HexPacked -> Serialize.print_hex Serialize.serialize_bytes_packed solver_ast
  );
  solver_ast, output, base_ast

let rec collect_results results =
  match results with
  | [] -> Ok []
  | Ok v :: rest ->
      (match collect_results rest with
        | Ok vs -> Ok (v :: vs)
        | Error e -> Error e)
  | Error e :: _ -> Error e
