(set-logic ALL)


(synth-fun top () (Seq Bool)
; declare nonterminals
(
	(status_code (Seq Bool))

)
; grammar rules
(
	(status_code (Seq Bool) ((Constant (Seq Bool))))

)
)



(check-synth)
