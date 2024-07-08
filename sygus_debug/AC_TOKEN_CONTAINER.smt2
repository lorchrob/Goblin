(set-logic ALL)

(declare-datatype _STUB0_GRAMMAR_ELEMENT (
	(_stub0_grammar_element_con)
))
(declare-datatype AC_TOKEN_CONTAINER (
	(ac_token_container_con0 (des4 _STUB0_GRAMMAR_ELEMENT) (des5 Int))
))

(synth-fun top () AC_TOKEN_CONTAINER
; declare nonterminals
(
	(ac_token_container AC_TOKEN_CONTAINER)
	(ac_id_length Int)
	(ac_token_element Int)
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT)
)
; grammar rules
(
	(ac_token_container AC_TOKEN_CONTAINER ((ac_token_container_con0 _stub0_grammar_element ac_token_element)))
	(ac_id_length Int ((Constant Int)))
	(ac_token_element Int ((Constant Int)))
	(_stub0_grammar_element _STUB0_GRAMMAR_ELEMENT (_stub0_grammar_element_con))
)
)

(define-fun c6 ((ac_token_container AC_TOKEN_CONTAINER)) Bool 
	(match ac_token_container (
		((ac_token_container_con0 _stub0_grammar_element ac_token_element)
		 true)
	))
)
(constraint (c6 top))

(check-synth)
