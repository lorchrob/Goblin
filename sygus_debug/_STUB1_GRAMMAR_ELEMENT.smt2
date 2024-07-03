(set-logic ALL)

(declare-datatype AUTH_ALGO (STUB

))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con0 (des12 (_ BitVec 16)))
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
	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT ((_stub1_grammar_element_con0 bv)))
	(auth_algo AUTH_ALGO (STUB
))
	(bv (_ BitVec 16) ((Constant (_ BitVec 16))))

)
)

(define-fun c13 ((_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)) Bool 
	(match _stub1_grammar_element (
		((_stub1_grammar_element_con0 bv)
		 (= bv #b0000000000000000))
	))
)
(constraint (c13 top))

(check-synth)
