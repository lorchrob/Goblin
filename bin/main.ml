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
    <RG_ELEMENT_ID_EXTENSION> :: BitVector(8) 
    { <RG_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 92); };


    <RG_ID_LIST> ::= <RG_ID> | <RG_ID> <RG_ID_LIST>;

    <RG_ID> :: BitVector(8);

    <AC_TOKEN_CONTAINER> ::= <AC_ELEMENT_ID> <AC_ID_LENGTH> <AC_ELEMENT_ID_EXTENSION> <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- int_to_bitvector(8, 0); };


    <AC_ELEMENT_ID> :: BitVector(8)
    { <AC_ELEMENT_ID> <- int_to_bitvector(8, 255); };

    <AC_ID_LENGTH> :: BitVector(8);

    <AC_ELEMENT_ID_EXTENSION> :: BitVector(8)
    { <AC_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 93); };

    <AC_TOKEN_ELEMENT> :: BitList;
    <SCALAR> :: BitList;
    <ELEMENT> :: BitList;
    <CONFIRM_HASH> :: BitVector(256);
    <SEND_CONFIRM_COUNTER> :: BitVector(16);
    "
  )


  