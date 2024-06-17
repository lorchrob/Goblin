open Sbf

(* Main method *)
let () = 
  let input_string = 
  "
  <S> ::= <List> <Length> { <Length> <- length(<List>); length(<List>) < 100; };
  <List> :: BitList;
  <Length> :: Int;

  <Test> :: Int { <Test> <- 5; };

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
  let _ = TypeChecker.check_types ctx prm ast in

  (* Step 3: Divide and conquer *)
  ()
  (* Step 4: Print to SyGuS language *)