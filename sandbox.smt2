(set-logic ALL)

(declare-fun A () (Seq Int))

(assert (= (seq.len A) 3))

(check-sat)
(get-model)