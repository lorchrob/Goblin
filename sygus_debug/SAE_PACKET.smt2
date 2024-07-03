(set-logic ALL)

(declare-datatype AUTH_ALGO (
	(_stub0_grammar_element_con0)
))
(declare-datatype STATUS_CODE (
	(_stub1_grammar_element_con0)
))
(declare-datatype SAE_PACKET (
	(sae_packet_con0 (des8 AUTH_ALGO) (des9 STATUS_CODE))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code STATUS_CODE)
	(auth_algo AUTH_ALGO)
	(bv (_ BitVec 16))

)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con0 auth_algo status_code)))
	(status_code STATUS_CODE (_stub1_grammar_element_con0))
	(auth_algo AUTH_ALGO (_stub0_grammar_element_con0))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)



(check-synth)
