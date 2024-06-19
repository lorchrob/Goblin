open Sbf

(*!! TODO: 
   1. Fix indexing 
   2. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   3. Syntax check on dot notation, make sure it's unambiguous 
   4. Infer types for nonterminals with production rules 
   5. Make sure each nonterminal has a path to termination
*)

(* Main function *)
let () = 
  let input_string = 
  "<SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE> 
   { 
   <AUTH_ALGO> <- int_to_bitvector(16, 0); 
   };

   <STATUS_CODE> :: BitVector(16);
   <AUTH_ALGO> :: BitVector(16);
  " in 
  let ppf = Format.std_formatter in
  Lib.print_newline ppf;
  let ast = Utils.parse input_string in 
  Ast.pp_print_ast ppf ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in

  (* Step 3: Compute dependencies *)
  let ast = AbstractDeps.abstract_dependencies ast in 

  (* Step 4: Divide and conquer *)
  let asts = DivideAndConquer.split_ast ast in 

  (* Step 5: Print to SyGuS language *)
  Format.fprintf ppf "\n";
  List.iter (Sygus.pp_print_ast ppf) asts