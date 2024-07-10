open Sbf

(* NOTES: 
  * <AC_TOKEN_CONTAINER> ::= <AC_ID_LENGTH>  <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- <AC_TOKEN_ELEMENT>; };

    <AC_ID_LENGTH> :: Int;
    <AC_TOKEN_ELEMENT> :: Int;

    The sygus_ast will forget constructor names, so the string "AC_TOKEN_ELEMENT" will always 
    be dangling once AC_TOKEN_ELEMENT has been assigned a concrete value by sygus. 
    So for dependency computation, we need info from the original grammar to find the index of the 
    correct subterm, and then retrieve the value. 

  * I think some of the length constraints are written informally in comments rather than in the grammar
  * Test length constraint with RG_ID_LIST more rigorously
*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
    "
    <S> ::= <A> <B> { <A> <- <B>; };

    <A> :: BitVector(2); 
    <B> :: BitVector(2) { <B> = 0b01; } ;
    "
    (* "
    <S> ::= <A> { <A> <- int_to_bitvector(8, length(0b00000)); } ;
    <A> :: BitVector(8);
    " *)
  )


  