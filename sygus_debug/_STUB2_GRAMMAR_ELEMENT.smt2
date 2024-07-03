(set-logic ALL)

(declare-datatype _STUB2_GRAMMAR_ELEMENT (
	(_stub2_grammar_element_con0 (des21 (_ BitVec 16)))
))

(synth-fun top () _STUB2_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)
	(bv1 (_ BitVec 16))
	(bv2 (_ BitVec 16))

)
; grammar rules
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT ((_stub2_grammar_element_con0 bv1)))
	(bv1 (_ BitVec 16) ((Constant (_ BitVec 16))))
	(bv2 (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c22 ((_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)) Bool 
	(match _stub2_grammar_element (
		((_stub2_grammar_element_con0 bv1)
		 (= bv1 #b0000000000000111))
	))
)
(constraint (c22 top))

(check-synth)
