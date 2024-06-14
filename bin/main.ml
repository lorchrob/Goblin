open Sbf

(* Main method *)
let () = 
  print_endline "Hello, world!";
  let input_string = 
  "
  <S> ::= <List> <Length> { <Length> <- length(<List>.<B>); length(<List>) < 100 };
  <List> :: BitList;
  <Length> :: Int;
  " in 
  let _ = Utils.parse input_string in 
  ()
  (* Step 1: Type checking *)
  (* Step 2: Divide and conquer *)
  (* Step 3: Print to SyGuS language *)