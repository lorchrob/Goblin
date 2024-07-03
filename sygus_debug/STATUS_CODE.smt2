(set-logic ALL)

(declare-datatype STATUS_CODE (
	(status_code_con (des2 (_ BitVec 16)))
))

(synth-fun top () STATUS_CODE
; declare nonterminals
(
	(status_code STATUS_CODE)
	(a (_ BitVec 16))

)
; grammar rules
(
	(status_code STATUS_CODE ((status_code_con a)))
	(a (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c3 ((status_code STATUS_CODE)) Bool 
	(match status_code (
		((status_code_con a)
		 (= a #b0000010000000000)) 
	))
)
(constraint (c3 top))

(check-synth)
