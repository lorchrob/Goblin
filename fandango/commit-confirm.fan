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

from z3 import BitVecVal, Not

where BitVecVal(int(str(<CONFIRM>.<STATUS_CODE>), 2), 16) == BitVecVal(0, 16) or BitVecVal(int(str(<CONFIRM>.<STATUS_CODE>), 2), 16) == BitVecVal(1, 16);

where BitVecVal(int(str(<AC_TOKEN_CONTAINER>.<AC_ID_LENGTH>), 2), 8) == BitVecVal((len(str(<AC_TOKEN_CONTAINER>.<AC_TOKEN_ELEMENT>)) // 8) + 1, 8);

where not (len(str(<AC_TOKEN_ELEMENT>)) == 0);

where BitVecVal(int(str(<AC_ELEMENT_ID_EXTENSION>), 2), 8) == BitVecVal(93, 8);

where BitVecVal(int(str(<AC_ELEMENT_ID>), 2), 8) == BitVecVal(255, 8);

where BitVecVal(int(str(<RG_ID>), 2), 16) == BitVecVal(20, 16) or BitVecVal(int(str(<RG_ID>), 2), 16) == BitVecVal(21, 16);

where BitVecVal(int(str(<REJECTED_GROUPS>.<RG_ID_LENGTH>), 2), 8) == BitVecVal((len(str(<REJECTED_GROUPS>.<RG_ID_LIST>)) // 8) + 1, 8);

where BitVecVal(int(str(<RG_ELEMENT_ID_EXTENSION>), 2), 8) == BitVecVal(92, 8);

where BitVecVal(int(str(<RG_ELEMENT_ID>), 2), 8) == BitVecVal(255, 8);

where Not(BitVecVal(int(str(<PASSWD_ID>), 2), 8) == BitVecVal(0, 8));

where BitVecVal(int(str(<PASSWD_ELEMENT_ID_EXTENSION>), 2), 8) == BitVecVal(33, 8);

where BitVecVal(int(str(<PASSWD_ELEMENT_ID>), 2), 8) == BitVecVal(255, 8);

where BitVecVal(int(str(<PASSWORD_IDENTIFIER>.<PASSWD_ID_LENGTH>), 2), 8) == BitVecVal((len(str(<PASSWORD_IDENTIFIER>.<PASSWD_ID>)) // 8) + 1, 8);

where BitVecVal(int(str(<CONFIRM>.<AUTH_ALGO>), 2), 16) == BitVecVal(3, 16) and BitVecVal(int(str(<CONFIRM>.<AUTH_SEQ_CONFIRM>), 2), 16) == BitVecVal(2, 16);

where BitVecVal(int(str(<COMMIT>.<STATUS_CODE>), 2), 16) == BitVecVal(0, 16) or BitVecVal(int(str(<COMMIT>.<STATUS_CODE>), 2), 16) == BitVecVal(1, 16) or BitVecVal(int(str(<COMMIT>.<STATUS_CODE>), 2), 16) == BitVecVal(126, 16);

where Not(BitVecVal(int(str(<COMMIT>.<STATUS_CODE>), 2), 16) == BitVecVal(1, 16));

where Not(BitVecVal(int(str(<COMMIT>.<STATUS_CODE>), 2), 16) == BitVecVal(0, 16));

where BitVecVal(19, 16) <= BitVecVal(int(str(<COMMIT>.<GROUP_ID>), 2), 16) and BitVecVal(int(str(<COMMIT>.<GROUP_ID>), 2), 16) <= BitVecVal(20, 16);

where BitVecVal(int(str(<COMMIT>.<AUTH_ALGO>), 2), 16) == BitVecVal(3, 16);

