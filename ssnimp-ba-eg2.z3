;; Command to run: 
;; z3 -smt2 smt.mbqi=true smt.macro-finder=true -T:20 ssnimp-ba-eg2.z3
(declare-datatypes () ((Oper GB WD DP NOP)))
(declare-datatypes () ((Eff e1 e2 e3 e4 e5)))
(declare-fun oper (Eff) Oper)
(declare-fun rval (Eff) Int)
; rval of deposits and withdraws is always non-negative
(assert (forall ((e Eff)) (=> (or (= (oper e) DP) (= (oper e) WD)) 
                              (>= (rval e) 0))))

;; ---- VISIBILITY ----
(declare-fun vis (Eff Eff) Bool)
;; vis is irreflexive
(assert (forall ((e Eff)) (not (vis e e))))

;; a1 and a2 are non-negative constants.
(declare-const a1 Int)
(declare-const a2 Int)
(assert (>= a1 0))
(assert (>= a2 0))
;; sel is the guarded value of rval per effect
(define-fun sel ((a Eff)(n Eff)) Int
  (if (and (vis a n) (= (oper a) DP)) (rval a) 
    (if (and (vis a n) (= (oper a) WD)) (- 0 (rval a)) 0)))
;; getBalance defined per effect
(define-fun getBalance ((n Eff)) Int
  (+ (sel e1 n) (sel e2 n) (sel e3 n) (sel e4 n) (sel e5 n)))
;; e3 is GB
(assert (= (oper e3) GB))
(assert (= (rval e3) (getBalance e3)))
;; e4 may be WD or NOP
(assert 
  (if (>= (getBalance e4) a1) 
    (and (= (oper e4) WD) (= (rval e4) a1)) 
    (and (= (oper e4) NOP))))
;; e5 may be WD or NOP
(assert 
  (if (>= (getBalance e5) a2) 
    (and (= (oper e5) WD) (= (rval e5) a2)) 
    (and (= (oper e5) NOP))))
;; getBalance returns non-negative value initially.
(assert (>= (+ (sel e1 e3) (sel e2 e3)) 0))

;; The high-level invariant
(declare-const inv Bool)
(assert (= inv (>= (rval e3) 0)))

;; assert soft constraints
(assert-soft (not (vis e1 e2)))
(assert-soft (not (vis e1 e3)))
(assert-soft (not (vis e1 e4)))
(assert-soft (not (vis e1 e5)))
(assert-soft (not (vis e2 e1)))
(assert-soft (not (vis e2 e3)))
(assert-soft (not (vis e2 e4)))
(assert-soft (not (vis e2 e5)))
(assert-soft (not (vis e3 e1)))
(assert-soft (not (vis e3 e2)))
(assert-soft (not (vis e3 e4)))
(assert-soft (not (vis e3 e5)))
(assert-soft (not (vis e4 e1)))
(assert-soft (not (vis e4 e2)))
(assert-soft (not (vis e4 e3)))
(assert-soft (not (vis e4 e5)))
(assert-soft (not (vis e5 e1)))
(assert-soft (not (vis e5 e2)))
(assert-soft (not (vis e5 e3)))
(assert-soft (not (vis e5 e4)))

;; Initial run - no more constraints
(push)
  (assert (not inv))
  (check-sat)    
  (get-model)
  (eval (oper e1))
  (eval (oper e2))
  (eval (oper e3))
  (eval (oper e4))
  (eval (oper e5))
(pop)

;; 1st Counterexample
(push)
  ;; First assert general details about effect labels.
  ;; e1 is DP, e2 is GB, e4 is WD, and e5 is NOP.
  (assert (= (oper e1) DP))
  (assert (= (oper e2) DP))
  (assert (= (oper e4) NOP))
  (assert (= (oper e5) WD))

  (push)
    ;; Then assert specific details.
    ;; a1 = 1 and a2 = 1
    (assert (and (= a1 1) (= a2 1)))
    ;; vis(e2,e5) /\ vis(e5,e3)
    (assert (and (vis e2 e5) (vis e5 e3)))
    (assert inv)
    (check-sat)    
    (get-model)
    ;; The solution returned is vis(e2,e3)
  (pop)

  ;; Now, check if the solution is sufficient
  (push)
    (assert (=> (and (vis e2 e5) (vis e5 e3)) (vis e2 e3)))
    (assert (not inv))
    (check-sat) ; Sat. So, the solution is insufficient.
    (get-model)
  (pop)

  (push)
    ;; Assert specific details for this counterexample.
    ;; a1 = 1 and a2 = 1
    (assert (and (= a1 1) (= a2 1)))
    ;; vis(e1,e5) /\ vis(e5,e3)
    (assert (and (vis e1 e5) (vis e5 e3)))
    ;; The following is needed to ensure that vis(e2,e3) is not
    ;; proposed as a solution. In general, there is a principle that 
    ;; avoids these kind of special cases.
    (assert-soft (not (vis e2 e3)) :weight 2)
    (assert inv)
    (check-sat)    
    (get-model)
    ;; The solution returned is vis(e1,e3)
  (pop)

  ;; Now, check if the solution is sufficient
  (push)
    (assert (=> (and (vis e2 e5) (vis e5 e3)) (vis e2 e3)))
    (assert (=> (and (vis e1 e5) (vis e5 e3)) (vis e1 e3)))
    (assert (not inv))
    (check-sat) ; Unsat! So, the solution is indeed sufficient.
    (get-model)
  (pop)
(pop)
;; End of counterexample 1

;; By generalizing the two solutions we learnt above (in practice, we
;; use more samples by increasing the number of effects), we arrive at
;; the following contract:
(assert (forall ((a Eff)(b Eff)(c Eff))
  (=> (and (= (oper a) DP) (= (oper b) WD) (= (oper c) GB) 
           (vis a b) (vis b c))
      (vis a c))))

;; Second run - with gb contract asserted.
(push)
  (assert (not inv))
  (check-sat)    
  (get-model)
  ;; This gives counterexample #2, where two withdraws happen
  ;; concurrently. Repeating the above process with this example 
  ;; gives the total order contract for withdraw.
  (eval (oper e1))
  (eval (oper e2))
  (eval (oper e3))
  (eval (oper e4))
  (eval (oper e5))
(pop)

;; 2nd Counterexample
(push)
  ;; First assert general details about effect labels.
  (assert (= (oper e1) WD))
  (assert (= (oper e2) DP))
  (assert (= (oper e4) WD))
  (assert (= (oper e5) NOP))

  (push)
    ;; Then assert specific details.
    ;; a1 = 1 and a2 = 1
    (assert (and (= a1 1) (= a2 1)))
    (assert (and (= (rval e1) 1) (= (rval e2) 1)))
    ;; vis(e1,e3) /\ vis(e2,e3) /\ vis(e4,e3) 
    ;; /\ vis(e2,e4)
    (assert (and (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e2 e4)))
    (assert inv)
    (check-sat) ;; Unsat! No solution exists!
    (get-model)
  (pop)
(pop)

;; Negate the counterexample and try.
(push)
 (assert (not (and 
  (= (oper e1) WD) (= (oper e2) DP) (= (oper e4) WD) 
  (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e2 e4))))
  (assert (not inv))
  (check-sat)
  (get-model)
  (eval (oper e1))
  (eval (oper e2))
  (eval (oper e3))
  (eval (oper e4))
  (eval (oper e5))
(pop)


;; 3rd Counterexample
(push)
  ;; First assert general details about effect labels.
  (assert (= (oper e1) DP))
  (assert (= (oper e2) WD))
  (assert (= (oper e4) NOP))
  (assert (= (oper e5) WD))

  (push)
    ;; Then assert specific details.
    ;; a1 = 1 and a2 = 1
    (assert (and (= a1 1) (= a2 1)))
    (assert (and (= (rval e1) 1) (= (rval e2) 1)))
    (assert (and (vis e1 e3) (vis e2 e3) (vis e5 e3) (vis e1 e5)))
    (assert inv)
    (check-sat) ;; Unsat! No solution exists!
    (get-model)
  (pop)
(pop)

;; Negate both the counterexamples and try.
(push)
 (assert (not (and 
  (= (oper e1) WD) (= (oper e2) DP) (= (oper e4) WD) 
  (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e2 e4))))
 (assert (not (and 
  (= (oper e1) DP) (= (oper e2) WD) (= (oper e5) WD) 
  (vis e1 e3) (vis e2 e3) (vis e5 e3) (vis e1 e5))))
  (assert (not inv))
  (check-sat)
  (get-model)
  (eval (oper e1))
  (eval (oper e2))
  (eval (oper e3))
  (eval (oper e4))
  (eval (oper e5))
(pop)

;; 4th Counterexample
(push)
  ;; First assert general details about effect labels.
  (assert (= (oper e1) DP))
  (assert (= (oper e2) WD))
  (assert (= (oper e4) WD))
  (assert (= (oper e5) NOP))

  (push)
    ;; Then assert specific details.
    ;; a1 = 1 and a2 = 1
    (assert (and (= a1 1) (= a2 1)))
    (assert (and (= (rval e1) 1) (= (rval e2) 1)))
    (assert (and (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e1 e4)))
    (assert inv)
    (check-sat) ;; Unsat! No solution exists!
    (get-model)
  (pop)
(pop)

;; Negate all counterexamples until Unsat (Note: all are unfixable).
(push)
  (assert (not (and 
    (= (oper e1) WD) (= (oper e2) DP) (= (oper e4) WD) 
    (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e2 e4))))
  (assert (not (and 
    (= (oper e1) DP) (= (oper e2) WD) (= (oper e5) WD) 
    (vis e1 e3) (vis e2 e3) (vis e5 e3) (vis e1 e5))))
  (assert (not (and 
    (= (oper e1) DP) (= (oper e2) WD) (= (oper e4) WD) 
    (vis e1 e3) (vis e2 e3) (vis e4 e3) (vis e1 e4))))
  (assert (not (and 
    (= (oper e1) WD) (= (oper e2) DP) (= (oper e5) WD) 
    (vis e1 e3) (vis e2 e3) (vis e5 e3) (vis e2 e5))))
  (assert (not (and 
    (= (oper e1) DP) (= (oper e4) WD) (= (oper e5) WD) 
    (vis e1 e3) (vis e4 e3) (vis e5 e3) (vis e1 e4) (vis e1 e5))))
  (assert (not (and 
    (= (oper e2) DP) (= (oper e4) WD) (= (oper e5) WD) 
    (vis e2 e3) (vis e4 e3) (vis e5 e3) (vis e2 e4) (vis e2 e5))))
  (assert (not inv))
  (check-sat) ; Unsat. No more counterexamples.
  (get-model)
  (eval (oper e1))
  (eval (oper e2))
  (eval (oper e3))
  (eval (oper e4))
  (eval (oper e5))
(pop)

;; Generalizing from counterexamples, we get the following contract:
(assert (forall ((a Eff)(b Eff)(c Eff)(d Eff)) 
  (=> (and (= (oper a) DP) (= (oper b) WD) (= (oper c) WD) (= (oper d) GB)
           (vis a b) (vis a c) (vis a d) (vis b d) (vis c d))
      (or (vis b c) (vis c b)))))
(check-sat)

(push)
  (assert (not inv))
  (check-sat)    
  (get-model) ;; Unsat! We have inferred all the contracts.
(pop)
