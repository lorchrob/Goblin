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

(define-fun c1 ((status_code (Seq Bool))) Bool 
	(> (seq.len status_code) 0)
)
(constraint (c1 top))

(check-synth)
