let num_packets = 3
let num_queues = 1

type packet_type = int


let nonterminals = ["AC_TOKEN" ; "RG_ID"; "RG_ID_LIST"; "REJECTED_GROUPS"; "RG_ID_LENGTH"; "PASSWD_ELEMENT_ID_EXTENSION"; "AC_ID_LENGTH"; "STATUS_CODE"; "RG_ELEMENT_ID_EXTENSION" ;"PASSWD_ID_LENGTH";"PASSWORD_IDENTIFIER"; "PASSWD_ELEMENT_ID" ; "RG_ELEMENT_ID"; "PASSWD_ID"; "AC_ELEMENT_ID"; "AC_TOKEN_ELEMENT"; "AC_TOKEN"; "COMMIT"; "CONFIRM"; "AUTH_SEQ_CONFIRM"; "AC_TOKEN_CONTAINER"; "CONFIRM_HASH"; "AUTH_ALGO"; "AUTH_SEQ_COMMIT"; "CONFIRM_HASH"; "SCALAR"; "GROUP_ID"; "ELEMENT"; "SEND_CONFIRM_COUNTER";]

let start_symbol = "SAE_PACKET"
(* let packet_types = List.init num_packets (fun x -> x) *)

(* let queue_handle = List.init num_queues (fun x -> x) *)

