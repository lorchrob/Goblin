(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con)
))
(declare-datatype AC_TOKEN_CONTAINER (
	(_stub2_grammar_element_con0)
))
(declare-datatype RG_ELEMENT_ID_EXTENSION (
	(rg_element_id_extension_con0 (des6 _STUB0_GRAMMAR_ELEMENT))
))

(synth-fun top () RG_ELEMENT_ID_EXTENSION
; declare nonterminals
(
	(rg_element_id_extension RG_ELEMENT_ID_EXTENSION)
	(ac_token_container AC_TOKEN_CONTAINER)
	(ac_id_length (_ BitVec 8))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(rg_element_id_extension RG_ELEMENT_ID_EXTENSION ((rg_element_id_extension_con0 _stub0_grammar_element)))
	(ac_token_container AC_TOKEN_CONTAINER (_stub2_grammar_element_con0))
	(ac_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT (_stub1_grammar_element_con))
)
)



(check-synth)
