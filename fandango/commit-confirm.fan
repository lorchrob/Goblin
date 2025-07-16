<start> ::= <SAE_PACKET> ; 
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

from bitstring import BitArray

where (BitArray(bin=str(<CONFIRM>.<STATUS_CODE>)).uint == 0) or (BitArray(bin=str(<CONFIRM>.<STATUS_CODE>)).uint == 1);

where BitArray(bin=str(<AC_TOKEN_CONTAINER>.<AC_ID_LENGTH>)).uint == (len(str(<AC_TOKEN_CONTAINER>.<AC_TOKEN_ELEMENT>)) // 8) + 1;

where not (len(str(<AC_TOKEN_ELEMENT>)) == 0);

where BitArray(bin=str(<AC_ELEMENT_ID_EXTENSION>)).uint == 93;

where BitArray(bin=str(<AC_ELEMENT_ID>)).uint == 255;

where (BitArray(bin=str(<RG_ID>)).uint == 20) or (BitArray(bin=str(<RG_ID>)).uint == 21);

where BitArray(bin=str(<REJECTED_GROUPS>.<RG_ID_LENGTH>)).uint == (len(str(<REJECTED_GROUPS>.<RG_ID_LIST>)) // 8) + 1;

where BitArray(bin=str(<RG_ELEMENT_ID_EXTENSION>)).uint == 92;

where BitArray(bin=str(<RG_ELEMENT_ID>)).uint == 255;

where not (BitArray(bin=str(<PASSWD_ID>)).uint == 0);

where BitArray(bin=str(<PASSWD_ELEMENT_ID_EXTENSION>)).uint == 33;

where BitArray(bin=str(<PASSWD_ELEMENT_ID>)).uint == 255;

where BitArray(bin=str(<PASSWORD_IDENTIFIER>.<PASSWD_ID_LENGTH>)).uint == (len(str(<PASSWORD_IDENTIFIER>.<PASSWD_ID>)) // 8) + 1;

where (BitArray(bin=str(<CONFIRM>.<AUTH_ALGO>)).uint == 3) and (BitArray(bin=str(<CONFIRM>.<AUTH_SEQ_CONFIRM>)).uint == 2);

where (BitArray(bin=str(<COMMIT>.<STATUS_CODE>)).uint == 0) or \
      (BitArray(bin=str(<COMMIT>.<STATUS_CODE>)).uint == 1) or \
      (BitArray(bin=str(<COMMIT>.<STATUS_CODE>)).uint == 126);

where not (BitArray(bin=str(<COMMIT>.<STATUS_CODE>)).uint == 1);

where not (BitArray(bin=str(<COMMIT>.<STATUS_CODE>)).uint == 0);

where 19 <= BitArray(bin=str(<COMMIT>.<GROUP_ID>)).uint <= 20;

where BitArray(bin=str(<COMMIT>.<AUTH_ALGO>)).uint == 3;

