(set-logic ALL)

(declare-datatype SAE_PACKET (
	(sae_packet_con0 (des0 (_ BitVec 16)) (des1 (_ BitVec 16)))
))

(synth-fun top () SAE_PACKET
; declare nonterminals
(
	(sae_packet SAE_PACKET)
	(status_code (_ BitVec 16))
	(auth_algo (_ BitVec 16))

)
; grammar rules
(
	(sae_packet SAE_PACKET ((sae_packet_con0 auth_algo status_code)))
	(status_code (_ BitVec 16) ((Constant (_ BitVec 16))))
	(auth_algo (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c2 ((sae_packet SAE_PACKET)) Bool 
	(match sae_packet (
		((sae_packet_con0 auth_algo status_code)
		 (= auth_algo #b0000000000001100))
	))
)
(constraint (c2 top))

(check-synth)