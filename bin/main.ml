open Sbf

(* TODO: 
   1. Collect stubs in some data structure and add them to sygus printing stuff
   2. Fix indexing 
   3. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   4. Syntax check on dot notation, make sure it's unambiguous 
   5. Infer types for nonterminals with production rules 
   6. Make sure each nonterminal has a path to termination
*)

(* Main function *)
let () = 
  (* Semantic constraint example *)
  let _ = 
      "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
         { <AUTH_ALGO> = int_to_bitvector(16, 12); };

         <STATUS_CODE> :: BitVector(16);
         <AUTH_ALGO> :: BitVector(16);
      "
  in

  (* Dependent term calculation example *)
  let _ = 
      "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
         { <AUTH_ALGO> <- int_to_bitvector(16, 12); };

         <STATUS_CODE> :: BitVector(16);
         <AUTH_ALGO> :: BitVector(16);
      "
  in 

  (* Divide and conquer example *)
  let input_string = 
   "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
   <STATUS_CODE> ::= <BV> { <BV> = 0b0000000000000000; };
   <AUTH_ALGO> ::= <BV> { <BV> = 0b0000000000000001; };
   <BV> :: BitVector(16);
   "
in 

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
  let dep_map, ast = AbstractDeps.abstract_dependencies ast in 
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

  (* Step 4.5: Prune grammars (both within grammars, and unreachable stubs) *)
  (* TODO *)
  (* let asts = [List.hd asts] in *)

  (* Step 5: Print to SyGuS language and call SyGuS engine *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "SyGuS translation:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  List.iter (fun ast -> Sygus.pp_print_ast ppf ctx dep_map ast; Lib.pp_print_newline ppf) asts;
  Lib.pp_print_newline ppf;
  
  (* Step 6: Call sygus engine *)
  Format.fprintf ppf "Calling SyGuS:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_outputs = List.map (Sygus.call_sygus ctx dep_map) asts in
  List.iter (Format.pp_print_string ppf) sygus_outputs;

  (* Step 6: Parse SyGuS output *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Parsing SyGuS output:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_asts = List.map Utils.parse_sygus sygus_outputs in
  List.iter (SygusAst.pp_print_sygus_ast ppf) sygus_asts;
  

  (* Step 7: Recombine to single AST *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Recombining to single AST:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_ast = Recombine.recombine sygus_asts in 
  SygusAst.pp_print_sygus_ast ppf sygus_ast;

  (* Step 8: Compute dependencies *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Computing dependencies:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let sygus_ast = ComputeDeps.compute_deps dep_map sygus_ast in 
  SygusAst.pp_print_sygus_ast ppf sygus_ast;

  (* Step 9: Serialize! *)
  Lib.pp_print_newline ppf;
  Format.fprintf ppf "Serializing:";
  Lib.pp_print_newline ppf;
  Format.pp_print_flush ppf ();
  let output = Utils.capture_output SygusAst.serialize sygus_ast in 
  Format.pp_print_string ppf output;
  