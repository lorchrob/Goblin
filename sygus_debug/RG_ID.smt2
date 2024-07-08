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
	(_stub3_grammar_element_con0)
))

(synth-fun top () (_ BitVec 8)
; declare nonterminals
(
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
	(rg_id (_ BitVec 8) ((Constant (_ BitVec 8))))
	(ac_token_container AC_TOKEN_CONTAINER (_stub3_grammar_element_con0))
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
