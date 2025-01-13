(set-logic ALL)

(declare-datatype S (
	(s_con0 (des0 Int) (des1 Int))
))

(declare-fun var () S)

(define-fun c2 ((s S)) Bool 
	(match s (
		((s_con0 a a)
		 (> a 0))
	))
)
(assert (c2 var))

(check-sat)
(get-model)
