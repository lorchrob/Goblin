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

  * <REJECTED_GROUPS> ::= <RG_ELEMENT_ID> <RG_ID_LENGTH> <RG_ELEMENT_ID_EXTENSION> <RG_ID_LIST>
    { <RG_ID_LENGTH> <- int_to_bitvector(8, length(<RG_ID_LIST>)); };

    RG_ID_LIST is a (manually defined in grammar) list of bitvectors, so built-in length function doesn't work. 
    Possible solutions:
    1. BV to bitlist conversion operator
    2. Built-in BVList type
    3. Generic <Nonterminal>* notation that generates a sequence of whatever <Nonterminal>'s type is
*)

(* Main function *)
let () = 
  ignore (Pipeline.main_pipeline 
    "
    <PASSWD_ID> :: BitList; // Arbitrary length depends on the the <PASSWD_ID_LENGTH> field 
    
    <REJECTED_GROUPS> ::= <RG_ELEMENT_ID> <RG_ID_LENGTH> <RG_ELEMENT_ID_EXTENSION> <RG_ID_LIST>;

    <RG_ELEMENT_ID> :: BitVector(8) 
{ <RG_ELEMENT_ID> <- int_to_bitvector(8, 255); };


    <RG_ID_LENGTH>   :: BitVector(8); 

    <RG_ELEMENT_ID_EXTENSION> :: BitVector(8) 
    { <RG_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 92); };
    
    <RG_ID_LIST> ::= <RG_ID> | <RG_ID> <RG_ID_LIST>;
     
    <RG_ID> :: BitVector(8);
    
    <AC_TOKEN_CONTAINER> ::= <AC_ELEMENT_ID> <AC_ID_LENGTH> <AC_ELEMENT_ID_EXTENSION>  <AC_TOKEN_ELEMENT>;
    
    <AC_ELEMENT_ID> :: BitVector(8) 
    { <AC_ELEMENT_ID> <- int_to_bitvector(8, 255); };
    
    <AC_ID_LENGTH> :: BitVector(8);
    
    <AC_ELEMENT_ID_EXTENSION> :: BitVector(8)
    { <AC_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 93); };
    
    <AC_TOKEN_ELEMENT> :: BitList;
    
    <SCALAR>     :: BitList;  
    
    <ELEMENT> :: BitList;  
    
    <CONFIRM_HASH> :: BitVector(256); 
    
    <SEND_CONFIRM_COUNTER> :: BitVector(16);
    
    "
  )


  