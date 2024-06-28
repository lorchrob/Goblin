(set-logic ALL)

(declare-datatype SAE_PACKET (
	(sae_packet_con (des2 (Seq Bool)) (des3 (Seq Bool)))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code (Seq Bool))
	(auth_algo (Seq Bool))

)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con auth_algo status_code)))
	(status_code (Seq Bool) ((Constant (Seq Bool))))
	(auth_algo (Seq Bool) ((Constant (Seq Bool))))

)
)



(check-synth)
