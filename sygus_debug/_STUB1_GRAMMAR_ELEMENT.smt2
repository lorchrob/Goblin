(set-logic ALL)

(declare-datatype AUTH_ALGO (
	(_stub2_grammar_element_con0)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con0 (des19 (_ BitVec 16)))
))

(synth-fun top () _STUB1_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)
	(auth_algo AUTH_ALGO)
	(bv1 (_ BitVec 16))
	(bv2 (_ BitVec 16))

)
; grammar rules
(
	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT ((_stub1_grammar_element_con0 bv2)))
	(auth_algo AUTH_ALGO (_stub2_grammar_element_con0))
	(bv1 (_ BitVec 16) ((Constant (_ BitVec 16))))
	(bv2 (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c20 ((_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)) Bool 
	(match _stub1_grammar_element (
		((_stub1_grammar_element_con0 bv2)
		 (= bv2 #b0000000000000011))
	))
)
(constraint (c20 top))

(check-synth)
