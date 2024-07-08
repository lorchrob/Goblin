(set-logic ALL)


(synth-fun top () (_ BitVec 16)
; declare nonterminals
(
	(auth_algo (_ BitVec 16))

)
; grammar rules
(
	(auth_algo (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c5 ((auth_algo (_ BitVec 16))) Bool 
	(= auth_algo #b0000000000000111)
)
(constraint (c5 top))

(check-synth)
