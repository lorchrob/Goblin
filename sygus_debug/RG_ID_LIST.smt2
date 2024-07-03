(set-logic ALL)

(declare-datatype RG_ID_LIST (
	(rg_id_list_con0 (des3 (_ BitVec 8)) (des4 (_ BitVec 8))) 
	(rg_id_list_con1 (des5 RG_ID_LIST))
))

(synth-fun top () RG_ID_LIST
; declare nonterminals
(
	(rg_id_list RG_ID_LIST)
	(rg_id (_ BitVec 8))

)
; grammar rules
(
	(rg_id_list RG_ID_LIST ((rg_id_list_con0 rg_id rg_id) (rg_id_list_con1 rg_id_list)))
	(rg_id (_ BitVec 8) ((Constant (_ BitVec 8))))

)
)



(check-synth)
