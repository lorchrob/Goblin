<SAE_PACKET> ::= <COMMIT> | <CONFIRM> ;
<COMMIT> ::= <AUTH_ALGO> <AUTH_SEQ_COMMIT> <STATUS_CODE> <GROUP_ID> <AC_TOKEN> <SCALAR> <ELEMENT> <PASSWORD_IDENTIFIER> <REJECTED_GROUPS> <AC_TOKEN_CONTAINER> ;
<AUTH_ALGO> ::= <bv16>  ; 
<CONFIRM> ::= <AUTH_ALGO> <AUTH_SEQ_CONFIRM> <STATUS_CODE> <SEND_CONFIRM_COUNTER> <CONFIRM_HASH>;
<GROUP_ID> ::= <bv16> ;
<AUTH_SEQ_COMMIT> ::= "0b0000000000000001" ;
<AUTH_SEQ_CONFIRM> ::= "0b0000000000000010" ;
<STATUS_CODE> ::= <bv16>;
<AC_TOKEN> ::= "<AC_TOKEN>"; 
<PASSWORD_IDENTIFIER> ::= <PASSWD_ELEMENT_ID> <PASSWD_ID_LENGTH> <PASSWD_ELEMENT_ID_EXTENSION> <PASSWD_ID> ;
<PASSWD_ELEMENT_ID> ::= <bv8> ;
<PASSWD_ID_LENGTH> ::= <bv8> ;
<PASSWD_ELEMENT_ID_EXTENSION> ::= <bv8> ;
<PASSWD_ID> ::= <bv8>; 
<REJECTED_GROUPS> ::= <RG_ELEMENT_ID> <RG_ID_LENGTH> <RG_ELEMENT_ID_EXTENSION> <RG_ID_LIST>;
<RG_ELEMENT_ID> ::= <bv8> ;
<RG_ID_LENGTH>   ::= <bv8>; 
<RG_ELEMENT_ID_EXTENSION> ::= <bv8> ;
<RG_ID_LIST> ::= <RG_ID> | <RG_ID> <RG_ID_LIST>;
<RG_ID> ::= <bv16> ;
<AC_TOKEN_CONTAINER> ::= <AC_ELEMENT_ID> <AC_ID_LENGTH> <AC_ELEMENT_ID_EXTENSION> <AC_TOKEN_ELEMENT>;
<AC_ELEMENT_ID> ::= <bv8> ;
<AC_ID_LENGTH> ::= <bv8> ;
<AC_ELEMENT_ID_EXTENSION> ::= <bv8> ;
<AC_TOKEN_ELEMENT> ::= "" | <bit> <AC_TOKEN_ELEMENT>;
<SCALAR> ::= "<SCALAR>"; 
<ELEMENT> ::= "<ELEMENT>"; 
<CONFIRM_HASH> ::= "<CONFIRM_HASH>"; 
<SEND_CONFIRM_COUNTER> ::= "<SEND_CONFIRM_COUNTER>";
<bv8> ::= <bit> <bit> <bit> <bit> <bit> <bit> <bit> <bit> ;
<bv16> ::= <bv8> <bv8> ;
<bit> ::= "0" | "1";

where  
  <CONFIRM>.<STATUS_CODE> = int_to_bitvector(16, 0) or
  <CONFIRM>.<STATUS_CODE> = int_to_bitvector(16, 1);

where 
  <AC_TOKEN_CONTAINER>.<AC_ID_LENGTH> = int_to_bitvector(8, (length(<AC_TOKEN_CONTAINER>.<AC_TOKEN_ELEMENT>)/8)+1); 

where 
  not (length(<AC_TOKEN_ELEMENT>) = 0) ;

where 
  <AC_ELEMENT_ID_EXTENSION> = int_to_bitvector(8, 93);

where
  <AC_ELEMENT_ID> = int_to_bitvector(8, 255); 

where <RG_ID> = int_to_bitvector(16, 20) or <RG_ID> = int_to_bitvector(16, 21); 

where <REJECTED_GROUPS>.<RG_ID_LENGTH> = int_to_bitvector(8, (length(<REJECTED_GROUPS>.<RG_ID_LIST>)/8)+1) ;

where <RG_ELEMENT_ID_EXTENSION> = int_to_bitvector(8, 92);

where <RG_ELEMENT_ID> = int_to_bitvector(8, 255);

where not (<PASSWD_ID> = int_to_bitvector(8, 0));

where <PASSWD_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 33); 

where <PASSWD_ELEMENT_ID> <- int_to_bitvector(8, 255); 

where <PASSWORD_IDENTIFIER>.<PASSWD_ID_LENGTH> <- int_to_bitvector(8, (length(<PASSWORD_IDENTIFIER>.<PASSWD_ID>)/8)+1); 

where <CONFIRM>.<AUTH_ALGO> <- int_to_bitvector(16, 3) and <CONFIRM>.<AUTH_SEQ_CONFIRM> <- int_to_bitvector(16, 2); 

where <COMMIT>.<STATUS_CODE> = int_to_bitvector(16, 0) or <COMMIT>.<STATUS_CODE> = int_to_bitvector(16, 1) or <COMMIT>.<STATUS_CODE> = int_to_bitvector(16, 126); 

where lnot (<COMMIT>.<STATUS_CODE> = int_to_bitvector(16, 1)) ;

where lnot (<COMMIT>.<STATUS_CODE> = int_to_bitvector(16, 0));

where int_to_bitvector(16, 19) bvlte <COMMIT>.<GROUP_ID> and <COMMIT>.<GROUP_ID> bvlte int_to_bitvector(16, 20); 

where <COMMIT>.<AUTH_ALGO> = int_to_bitvector(16, 3);
