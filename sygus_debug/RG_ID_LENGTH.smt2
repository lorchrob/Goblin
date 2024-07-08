(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con)
))
(declare-datatype _STUB1_GRAMMAR_ELEMENT (
	(_stub1_grammar_element_con)
))
(declare-datatype _STUB2_GRAMMAR_ELEMENT (
	(_stub2_grammar_element_con)
))
(declare-datatype AC_ELEMENT_ID_EXTENSION (
	(ac_element_id_extension_con0 (des13 _STUB2_GRAMMAR_ELEMENT))
))
(declare-datatype AC_ELEMENT_ID (
	(ac_element_id_con0 (des14 _STUB1_GRAMMAR_ELEMENT))
))
(declare-datatype AC_TOKEN_CONTAINER (
	(ac_token_container_con0 (des15 AC_ELEMENT_ID) (des16 (_ BitVec 8)) (des17 AC_ELEMENT_ID_EXTENSION) (des18 (Seq Bool)))
))
(declare-datatype RG_ID_LIST (
	(rg_id_list_con0 (des19 (_ BitVec 8))) 
	(rg_id_list_con1 (des20 (_ BitVec 8)) (des21 RG_ID_LIST))
))
(declare-datatype RG_ELEMENT_ID_EXTENSION (
	(rg_element_id_extension_con0 (des22 _STUB0_GRAMMAR_ELEMENT))
))

(synth-fun top () (_ BitVec 8)
; declare nonterminals
(
	(rg_id_length (_ BitVec 8))
	(rg_element_id_extension RG_ELEMENT_ID_EXTENSION)
	(rg_id_list RG_ID_LIST)
	(rg_id (_ BitVec 8))
	(ac_token_container AC_TOKEN_CONTAINER)
	(ac_element_id AC_ELEMENT_ID)
	(ac_id_length (_ BitVec 8))
	(ac_element_id_extension AC_ELEMENT_ID_EXTENSION)
	(ac_token_element (Seq Bool))
	(scalar (Seq Bool))
	(element (Seq Bool))
	(confirm_hash (_ BitVec 256))
	(send_confirm_counter (_ BitVec 16))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(rg_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(rg_element_id_extension RG_ELEMENT_ID_EXTENSION ((rg_element_id_extension_con0 _stub0_grammar_element)))
	(rg_id_list RG_ID_LIST ((rg_id_list_con0 rg_id) (rg_id_list_con1 rg_id rg_id_list)))
	(rg_id (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_token_container AC_TOKEN_CONTAINER ((ac_token_container_con0 ac_element_id ac_id_length ac_element_id_extension ac_token_element)))
	(ac_element_id AC_ELEMENT_ID ((ac_element_id_con0 _stub1_grammar_element)))
	(ac_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_element_id_extension AC_ELEMENT_ID_EXTENSION ((ac_element_id_extension_con0 _stub2_grammar_element)))
	(ac_token_element (Seq Bool) ((Constant (Seq Bool))))
	(scalar (Seq Bool) ((Constant (Seq Bool))))
	(element (Seq Bool) ((Constant (Seq Bool))))
	(confirm_hash (_ BitVec 256) ((Constant (_ BitVec 256))))
	(send_confirm_counter (_ BitVec 16) ((Constant (_ BitVec 16))))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT (_stub1_grammar_element_con))	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT (_stub2_grammar_element_con))
)
)



(check-synth)
