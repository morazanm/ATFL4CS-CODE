#lang fsm

;; L(M) = ab*
(define M (make-dfa `(S F ,DEAD)
                    '(a b)
                    'S
                    '(F)
                    `((S a F)
                      (S b ,DEAD)
                      (F a ,DEAD)
                      (F b F)
                      (,DEAD a ,DEAD)
                      (,DEAD b ,DEAD))
                    'no-dead))

(check-equal? (sm-apply M '()) 'reject)
(check-equal? (sm-apply M '(b b b)) 'reject)
(check-equal? (sm-apply M '(a b b b a b)) 'reject)
(check-equal? (sm-apply M '(a b)) 'accept)
(check-equal? (sm-apply M '(a b b b b)) 'accept)

;; L(M) = abb*
(define M2 (make-dfa `(S A F ,DEAD)
                     '(a b)
                     'S
                     '(F)
                     `((S a A)
                       (S b ,DEAD)
                       (A a ,DEAD)
                       (A b F)
                       (F a ,DEAD)
                       (F b F)
                       (,DEAD a ,DEAD)
                       (,DEAD b ,DEAD))
                     'no-dead))

(check-equal? (sm-apply M '()) 'reject)
(check-equal? (sm-apply M '(b b b)) 'reject)
(check-equal? (sm-apply M '(a b b b a b)) 'reject)
(check-equal? (sm-apply M '(a b)) 'accept)
(check-equal? (sm-apply M '(a b b b b)) 'accept)

