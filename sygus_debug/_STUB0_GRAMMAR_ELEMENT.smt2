(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con0 (des17 (_ BitVec 16)))
))

(synth-fun top () _STUB0_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)
	(bv1 (_ BitVec 16))
	(bv2 (_ BitVec 16))

)
; grammar rules
(
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT ((_stub0_grammar_element_con0 bv1)))
	(bv1 (_ BitVec 16) ((Constant (_ BitVec 16))))
	(bv2 (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c18 ((_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)) Bool 
	(match _stub0_grammar_element (
		((_stub0_grammar_element_con0 bv1)
		 (= bv1 #b0000000000000111))
	))
)
(constraint (c18 top))

(check-synth)
