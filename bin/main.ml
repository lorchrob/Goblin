open Sbf

(*!! TODO: 
   1. Fix indexing 
   2. Syntax checks on case expressions (make sure they're valid cases and exhaustive)
   3. Syntax check on dot notation, make sure it's unambiguous 
   4. Infer types for nonterminals with production rules *)

(* Main function *)
let () = 
  let input_string = 
  "
  <S> ::= <List> <Length> { <Length> <- length(<List>); length(<List>) < 100; };
  <List> :: BitList;
  <Length> :: Int;

  <Test> :: Int { <Test> <- 5; };
  <Test2> :: Int { <Test2> <- 5; };

  <SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
  " in 
  let ast = Utils.parse input_string in 
  Ast.pp_print_ast Format.std_formatter ast;

  (* Step 1: Syntactic checks *)
  let prm = SyntaxChecker.build_prm ast in
  let nt_set = SyntaxChecker.build_nt_set ast in
  let ast = SyntaxChecker.check_syntax prm nt_set ast in 

  (* Step 2: Type checking *)
  let ast, ctx = TypeChecker.build_context ast in
  let ast = TypeChecker.check_types ctx ast in

  (* Step 3: Divide and conquer *)
  let asts = DivideAndConquer.split_ast ast in 

  (* Step 4: Print to SyGuS language *)
  List.iter Sygus.pp_print_ast asts