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
(declare-datatype _STUB3_GRAMMAR_ELEMENT (
	(_stub3_grammar_element_con)
))
(declare-datatype AC_ELEMENT_ID_EXTENSION (
	(ac_element_id_extension_con0 (des19 _STUB3_GRAMMAR_ELEMENT))
))
(declare-datatype AC_ELEMENT_ID (
	(ac_element_id_con0 (des20 _STUB2_GRAMMAR_ELEMENT))
))
(declare-datatype AC_TOKEN_CONTAINER (
	(ac_token_container_con0 (des21 AC_ELEMENT_ID) (des22 (_ BitVec 8)) (des23 AC_ELEMENT_ID_EXTENSION) (des24 (Seq Bool)))
))
(declare-datatype RG_ID_LIST (
	(rg_id_list_con0 (des25 (_ BitVec 8))) 
	(rg_id_list_con1 (des26 (_ BitVec 8)) (des27 RG_ID_LIST))
))
(declare-datatype RG_ELEMENT_ID_EXTENSION (
	(rg_element_id_extension_con0 (des28 _STUB1_GRAMMAR_ELEMENT))
))
(declare-datatype RG_ELEMENT_ID (
	(rg_element_id_con0 (des29 _STUB0_GRAMMAR_ELEMENT))
))
(declare-datatype REJECTED_GROUPS (
	(rejected_groups_con0 (des30 RG_ELEMENT_ID) (des31 (_ BitVec 8)) (des32 RG_ELEMENT_ID_EXTENSION) (des33 RG_ID_LIST))
))

(synth-fun top () (Seq Bool)
; declare nonterminals
(
	(passwd_id (Seq Bool))
	(rejected_groups REJECTED_GROUPS)
	(rg_element_id RG_ELEMENT_ID)
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
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT)	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT)	(_stub3_grammar_element _STUB3_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(passwd_id (Seq Bool) ((Constant (Seq Bool))))
	(rejected_groups REJECTED_GROUPS ((rejected_groups_con0 rg_element_id rg_id_length rg_element_id_extension rg_id_list)))
	(rg_element_id RG_ELEMENT_ID ((rg_element_id_con0 _stub0_grammar_element)))
	(rg_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(rg_element_id_extension RG_ELEMENT_ID_EXTENSION ((rg_element_id_extension_con0 _stub1_grammar_element)))
	(rg_id_list RG_ID_LIST ((rg_id_list_con0 rg_id) (rg_id_list_con1 rg_id rg_id_list)))
	(rg_id (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_token_container AC_TOKEN_CONTAINER ((ac_token_container_con0 ac_element_id ac_id_length ac_element_id_extension ac_token_element)))
	(ac_element_id AC_ELEMENT_ID ((ac_element_id_con0 _stub2_grammar_element)))
	(ac_id_length (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_element_id_extension AC_ELEMENT_ID_EXTENSION ((ac_element_id_extension_con0 _stub3_grammar_element)))
	(ac_token_element (Seq Bool) ((Constant (Seq Bool))))
	(scalar (Seq Bool) ((Constant (Seq Bool))))
	(element (Seq Bool) ((Constant (Seq Bool))))
	(confirm_hash (_ BitVec 256) ((Constant (_ BitVec 256))))
	(send_confirm_counter (_ BitVec 16) ((Constant (_ BitVec 16))))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))	(_stub1_grammar_element _STUB1_GRAMMAR_ELEMENT (_stub1_grammar_element_con))	(_stub2_grammar_element _STUB2_GRAMMAR_ELEMENT (_stub2_grammar_element_con))	(_stub3_grammar_element _STUB3_GRAMMAR_ELEMENT (_stub3_grammar_element_con))
)
)



(check-synth)
