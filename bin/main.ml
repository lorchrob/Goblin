open Sbf

(* NOTES: 
   
   * StubbedElements cannot use a stub_id, because other grammar rules have to refer to 
     them, and the other grammar rules are not aware of the stub_id
*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
  "
  <SAE_PACKET> ::= <AUTH_ALGO> <STATUS_CODE>;
  <STATUS_CODE> :: BitVector(16);
  <AUTH_ALGO> :: BitVector(16) { <AUTH_ALGO> = 0b0000000000000111; };
  "

    (* "
  <AC_ELEMENT_ID_EXTENSION> :: BitVector(8)
  { <AC_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 93); };

  <AC_TOKEN_ELEMENT> :: BitList;
  <SCALAR> :: BitList;
  <ELEMENT> :: BitList;
  <CONFIRM_HASH> :: BitVector(256);
  <SEND_CONFIRM_COUNTER> :: BitVector(16);
  " *)
  )


  