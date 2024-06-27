(set-logic ALL)

(declare-datatype _STUB2_GRAMMAR_ELEMENT (
	(_stub2_grammar_element_con (des12 (_ BitVec 16)))
))

(synth-fun top () _STUB2_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)
	(bv (_ BitVec 16))

)
; grammar rules
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT ((_stub2_grammar_element_con bv)))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c13 ((_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)) Bool 
	(match _stub2_grammar_element (
		((_stub2_grammar_element_con bv)
		 (= bv #b0000000000000001)) 
	))
)
(constraint (c13 top))

(check-synth)
