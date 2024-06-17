open Sbf

(* Main method *)
let () = 
  print_endline "Hello, world!";
  let input_string = 
  "
  <S> ::= <List> <Length> { <Length> <- length(<List>); length(<List>) < 100; };
  <List> :: BitList;
  <Length> :: Int;

  <SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
  " in 
  let ast = Utils.parse input_string in 
  Ast.pp_print_spec Format.std_formatter ast;
  ()

  (* Step 1: Type checking *)
  (* Step 2: Divide and conquer *)
  (* Step 3: Print to SyGuS language *)