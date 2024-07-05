(set-logic ALL)

(declare-datatype _STUB0_AC_ELEMENT_ID_EXTENSION (
	(_stub0_ac_element_id_extension_con)
))

(synth-fun top () _STUB0_AC_ELEMENT_ID_EXTENSION
; declare nonterminals
(
	(ac_token_element (Seq Bool))
	(scalar (Seq Bool))
	(element (Seq Bool))
	(confirm_hash (_ BitVec 256))
	(send_confirm_counter (_ BitVec 16))
	(_stub0_ac_element_id_extension _STUB0_AC_ELEMENT_ID_EXTENSION)
)
; grammar rules
(
	(ac_token_element (Seq Bool) ((Constant (Seq Bool))))
	(scalar (Seq Bool) ((Constant (Seq Bool))))
	(element (Seq Bool) ((Constant (Seq Bool))))
	(confirm_hash (_ BitVec 256) ((Constant (_ BitVec 256))))
	(send_confirm_counter (_ BitVec 16) ((Constant (_ BitVec 16))))
	(_stub0_ac_element_id_extension _STUB0_AC_ELEMENT_ID_EXTENSION (_stub0_ac_element_id_extension_con))
)
)



(check-synth)
