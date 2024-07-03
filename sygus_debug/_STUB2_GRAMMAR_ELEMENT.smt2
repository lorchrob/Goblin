(set-logic ALL)

(declare-datatype _STUB2_GRAMMAR_ELEMENT (
	(_stub2_grammar_element_con0 (des14 (_ BitVec 16)))
))

(synth-fun top () _STUB2_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)
	(bv (_ BitVec 16))

)
; grammar rules
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT ((_stub2_grammar_element_con0 bv)))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c15 ((_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)) Bool 
	(match _stub2_grammar_element (
		((_stub2_grammar_element_con0 bv)
		 (= bv #b0000000000000001))
	))
)
(constraint (c15 top))

(check-synth)
