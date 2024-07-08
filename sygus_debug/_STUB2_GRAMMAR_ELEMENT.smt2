(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con)
))
(declare-datatype _STUB2_GRAMMAR_ELEMENT (
	(_stub2_grammar_element_con0 (des7 _STUB1_GRAMMAR_ELEMENT))
))

(synth-fun top () _STUB2_GRAMMAR_ELEMENT
; declare nonterminals
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)
	(ac_id_length (_ BitVec 8))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT ((_stub2_grammar_element_con0 _stub1_grammar_element)))
	(ac_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT (_stub1_grammar_element_con))
)
)

(define-fun c8 ((_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)) Bool 
	(match _stub2_grammar_element (
		((_stub2_grammar_element_con0 _stub1_grammar_element)
		 true)
	))
)
(constraint (c8 top))

(check-synth)
