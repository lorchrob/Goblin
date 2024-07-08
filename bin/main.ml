open Sbf

(* NOTES: 
   
   * StubbedElements cannot use a stub_id, because other grammar rules have to refer to 
     them, and the other grammar rules are not aware of the stub_id

  * <AC_TOKEN_CONTAINER> ::= <AC_ELEMENT_ID> <AC_ID_LENGTH> <AC_ELEMENT_ID_EXTENSION> <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- int_to_bitvector(8, length(<AC_TOKEN_ELEMENT>)); };

    Need to support having a general expression as second arg to int_to_bitvector(.,.)
*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
    "
    <AC_TOKEN_CONTAINER> ::= <AC_ID_LENGTH>  <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- <AC_TOKEN_ELEMENT>; };

    <AC_ID_LENGTH> :: Int;
    <AC_TOKEN_ELEMENT> :: Int;
    "
  )


  