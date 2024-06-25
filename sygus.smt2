(set-logic ALL)

(declare-datatype _STUB0_AUTH_ALGO (
	(_stub0_auth_algo_con)
))
(declare-datatype SAE_PACKET (
	(sae_packet_con (des2 _STUB0_AUTH_ALGO) (des3 (_ BitVec 16)))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code (_ BitVec 16))
	(auth_algo (_ BitVec 16))
	(_stub0_auth_algo _STUB0_AUTH_ALGO)
)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con _stub0_auth_algo status_code)))
	(status_code (_ BitVec 16) ((Constant (_ BitVec 16))))
	(auth_algo (_ BitVec 16) ((Constant (_ BitVec 16))))
	(_stub0_auth_algo _STUB0_AUTH_ALGO (_stub0_auth_algo_con))
)
)



(check-synth)
