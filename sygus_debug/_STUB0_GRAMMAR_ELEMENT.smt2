(set-logic ALL)


(synth-fun top () (_ BitVec 16)
; declare nonterminals
(
	(_stub0_grammar_element (_ BitVec 16))

)
; grammar rules
(
	(_stub0_grammar_element (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c6 ((_stub0_grammar_element (_ BitVec 16))) Bool 
	(= auth_algo #b0000000000000111)
)
(constraint (c6 top))

(check-synth)
