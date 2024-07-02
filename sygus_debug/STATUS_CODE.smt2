(set-logic ALL)


(synth-fun top () (_ BitVec 16)
; declare nonterminals
(
	(status_code (_ BitVec 16))

)
; grammar rules
(
	(status_code (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)



(check-synth)
