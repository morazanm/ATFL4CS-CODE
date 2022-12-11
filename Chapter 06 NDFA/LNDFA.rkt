#lang fsm

;; States
;; S the consumed input is empty, starting state
;; F the consumed input is empty, final state
;; B the consumed input only contains a, final state
;; A the consumed input starts with a and ends with 0 or more b, final state

;; L = {ε} U aa* U ab*
(define LNDFA (make-ndfa '(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A b A)
                           (B a B))))

;; Tests for LNDFA
(check-equal? (sm-apply LNDFA '(a b a)) 'reject)
(check-equal? (sm-apply LNDFA '(b b b b b)) 'reject)
(check-equal? (sm-apply LNDFA '(a b b b b a a a)) 'reject)
(check-equal? (sm-apply LNDFA '()) 'accept)
(check-equal? (sm-apply LNDFA '(a)) 'accept)
(check-equal? (sm-apply LNDFA '(a a a a)) 'accept)
(check-equal? (sm-apply LNDFA '(a b b)) 'accept)



;; L = {ε} U aa* U ab*
(define LNDFA2 (make-ndfa `(S A B F ,DEAD)
                          '(a b)
                          'S
                          '(A B F)
                          `((S a A)
                            (S a B)
                            (S ,EMP F)
                            (A b A)
                            (B a B)
                            (S b ,DEAD)
                            (A a ,DEAD)
                            (B b ,DEAD)
                            (F a ,DEAD)
                            (F b ,DEAD))))

;; Tests for LNDFA2
(check-equal? (sm-apply LNDFA2 '(a b a)) 'reject)
(check-equal? (sm-apply LNDFA2 '(b b b b b)) 'reject)
(check-equal? (sm-apply LNDFA2 '(a b b b b a a a)) 'reject)
(check-equal? (sm-apply LNDFA2 '()) 'accept)
(check-equal? (sm-apply LNDFA2 '(a)) 'accept)
(check-equal? (sm-apply LNDFA2 '(a a a a)) 'accept)
(check-equal? (sm-apply LNDFA2 '(a b b)) 'accept)

;(define M4 (sm-graph L))


