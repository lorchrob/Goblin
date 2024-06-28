(set-logic ALL)

(declare-datatype AUTH_ALGO (
	(_stub2_grammar_element_con)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con (des20 (_ BitVec 16)))
))

(synth-fun top () _STUB1_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)
	(auth_algo AUTH_ALGO)
	(bv (_ BitVec 16))

)
; grammar rules
(
	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT ((_stub1_grammar_element_con bv)))
	(auth_algo AUTH_ALGO (_stub2_grammar_element_con))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c21 ((_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)) Bool 
	(match _stub1_grammar_element (
		((_stub1_grammar_element_con bv)
		 (= bv #b0000000000000000)) 
	))
)
(constraint (c21 top))

(check-synth)
