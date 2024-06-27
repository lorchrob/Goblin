(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con (des14 (_ BitVec 16)))
))

(synth-fun top () _STUB0_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)
	(bv (_ BitVec 16))

)
; grammar rules
(
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT ((_stub0_grammar_element_con bv)))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c15 ((_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)) Bool 
	(match _stub0_grammar_element (
		((_stub0_grammar_element_con bv)
		 (= bv #b0000000000000001)) 
	))
)
(constraint (c15 top))

(check-synth)
