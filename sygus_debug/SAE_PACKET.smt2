(set-logic ALL)

(declare-datatype SAE_PACKET (
	(sae_packet_con (des4 (Seq Bool)) (des5 (Seq Bool)))
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

(define-fun c6 ((sae_packet SAE_PACKET)) Bool 
	(match sae_packet (
		((sae_packet_con auth_algo status_code)
		 (> (seq.len auth_algo) 1)) 
	))
)
(constraint (c6 top))(define-fun c7 ((sae_packet SAE_PACKET)) Bool 
	(match sae_packet (
		((sae_packet_con auth_algo status_code)
		 (> (seq.len status_code) 5)) 
	))
)
(constraint (c7 top))

(check-synth)
