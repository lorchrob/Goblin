(set-logic ALL)

(declare-datatype AUTH_ALGO (
	(_stub0_grammar_element_con0)
))
(declare-datatype SAE_PACKET (
	(sae_packet_con0 (des4 (_ BitVec 16)) (des5 (_ BitVec 16)))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code (_ BitVec 16))
	(auth_algo AUTH_ALGO)

)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con0 auth_algo status_code)))
	(status_code (_ BitVec 16) ((Constant (_ BitVec 16))))
	(auth_algo AUTH_ALGO (_stub0_grammar_element_con0))

)
)



(check-synth)
