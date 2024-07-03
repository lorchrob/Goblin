(set-logic ALL)

(declare-datatype AUTH_ALGO (
	(_stub0_grammar_element_con0)
))
(declare-datatype STATUS_CODE (
	(status_code_con0 (des13 (_ BitVec 16)) (des14 (_ BitVec 16))) 
	(_stub1_grammar_element_con1)
))
(declare-datatype SAE_PACKET (
	(sae_packet_con0 (des15 AUTH_ALGO) (des16 STATUS_CODE))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code STATUS_CODE)
	(auth_algo AUTH_ALGO)
	(bv1 (_ BitVec 16))
	(bv2 (_ BitVec 16))

)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con0 auth_algo status_code)))
	(status_code STATUS_CODE ((status_code_con0 bv1 bv2) _stub1_grammar_element_con1))
	(auth_algo AUTH_ALGO (_stub0_grammar_element_con0))
	(bv1 (_ BitVec 16) ((Constant (_ BitVec 16))))
	(bv2 (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)



(check-synth)
