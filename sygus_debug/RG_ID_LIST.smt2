(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con)
))
(declare-datatype AC_ELEMENT_ID (
	(ac_element_id_con0 (des13 _STUB1_GRAMMAR_ELEMENT))
))
(declare-datatype AC_TOKEN_CONTAINER (
	(_stub2_grammar_element_con0)
))
(declare-datatype RG_ID_LIST (
	(rg_id_list_con0 (des14 (_ BitVec 8))) 
	(rg_id_list_con1 (des15 (_ BitVec 8)) (des16 RG_ID_LIST))
))

(synth-fun top () RG_ID_LIST
; declare nonterminals
(
	(rg_id_list RG_ID_LIST)
	(rg_id (_ BitVec 8))
	(ac_token_container AC_TOKEN_CONTAINER)
	(ac_element_id AC_ELEMENT_ID)
	(ac_id_length (_ BitVec 8))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(rg_id_list RG_ID_LIST ((rg_id_list_con0 rg_id) (rg_id_list_con1 rg_id rg_id_list)))
	(rg_id (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_token_container AC_TOKEN_CONTAINER (_stub2_grammar_element_con0))
	(ac_element_id AC_ELEMENT_ID ((ac_element_id_con0 _stub1_grammar_element)))
	(ac_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT (_stub1_grammar_element_con))
)
)



(check-synth)
