(declare-datatypes () ((Node A B C D)))

(define-fun edge ((a Node) (b Node)) Bool
	(ite (and (= a A) (= b B)) true
	(ite (and (= a B) (= b C)) true
	(ite (and (= a A) (= b C)) true
	(ite (and (= a C) (= b D)) true
	false)))))

(declare-fun path (Node Node) Bool)
(assert (forall ((a Node) (b Node)) (=> (edge a b) (path a b))))
(assert (forall ((a Node) (b Node) (c Node))
  (=> (and (edge a b) (path b c)) (path a c))))

(check-sat)
(get-model)
