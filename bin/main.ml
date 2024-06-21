open Sbf

(*!! TODO: 
   1. Collect stubs in some data structure and add them to sygus printing stuff
   2. Fix indexing 
   3. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   4. Syntax check on dot notation, make sure it's unambiguous 
   5. Infer types for nonterminals with production rules 
   6. Make sure each nonterminal has a path to termination
*)

(* Main function *)
let () = 
  let _ = 
      "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
         { <AUTH_ALGO> = int_to_bitvector(16, 12); };

         <STATUS_CODE> :: BitVector(16);
         <AUTH_ALGO> :: BitVector(16);
      "
  in
  let input_string = 
      "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
         { <AUTH_ALGO> <- int_to_bitvector(16, 12); };

         <STATUS_CODE> :: BitVector(16);
         <AUTH_ALGO> :: BitVector(16);
      "
  in 

  (* Step 0: Parse user input *)
  let ppf = Format.std_formatter in
  Lib.print_newline ppf;
  let ast = Utils.parse input_string in 
  Ast.pp_print_ast ppf ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 
  Lib.print_newline ppf;
  Format.fprintf ppf "Syntactic checks complete";
  Lib.print_newline ppf;

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in
  Lib.print_newline ppf;
  Format.fprintf ppf "Type checking complete";
  Lib.print_newline ppf;

  (* Step 3: Abstract away dependent terms in the grammar *)
  (* IN PROGRESS *)
  let dep_map, ast = AbstractDeps.abstract_dependencies ast in 

  (* Step 4: Divide and conquer *)
  (* TODO *)
  let asts = DivideAndConquer.split_ast ast in 

  (* Step 5: Print to SyGuS language and call SyGuS engine *)
  Lib.print_newline ppf;
  List.iter (Sygus.pp_print_ast ppf ctx dep_map) asts

  (* Step 6: Parse SyGuS output *)
  (* Fancy option: Create a parser programmatically based on the input grammar *)
  (* Easier solution: Parse into Lisp-like AST where each node has a name and a list of children. 
                      Should be enough to compute dependent terms and serialize. *)
  (* TODO *)

  (* Step 7: Recombine to single AST *)
  (* TODO *)

  (* Step 8: Compute dependencies *)
  (* TODO *)

  (* Step 9: Serialize! *)
  (* TODO *)