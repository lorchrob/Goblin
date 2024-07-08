open Sbf

(* NOTES: 
   
   * StubbedElements cannot use a stub_id, because other grammar rules have to refer to 
     them, and the other grammar rules are not aware of the stub_id

  * <AC_TOKEN_CONTAINER> ::= <AC_ID_LENGTH>  <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- <AC_TOKEN_ELEMENT>; };

    <AC_ID_LENGTH> :: Int;
    <AC_TOKEN_ELEMENT> :: Int;

    The sygus_ast will forget constructor names, so the string "AC_TOKEN_ELEMENT" will always 
    be dangling once AC_TOKEN_ELEMENT has been assigned a concrete value by sygus. 
    So for dependency computation, we need info from the original grammar to find the index of the 
    correct subterm, and then retrieve the value. 
*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
    "
    <RG_ELEMENT_ID_EXTENSION> :: BitVector(8) 
    { <RG_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 92); };
    
    <AC_TOKEN_CONTAINER> ::= <AC_ID_LENGTH> 
    { <AC_ID_LENGTH> <- 0b00000000; };
    
    <AC_ID_LENGTH> :: BitVector(8);
    "
  )


  