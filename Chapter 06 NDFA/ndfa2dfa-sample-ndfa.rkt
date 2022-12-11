#lang fsm

;; L = (aba)* U (ab)*
(define ND (make-ndfa '(S A B C D E)
                      '(a b)
                      'S
                      '(S)
                      `((S a A)
                        (S a B)
                        (A b C)
                        (B b D)
                        (C a E)
                        (D ,EMP S)
                        (E ,EMP S))))

;; Tests for M
(check-equal? (sm-apply ND '(b)) 'reject)
(check-equal? (sm-apply ND '(a a b a)) 'reject)
(check-equal? (sm-apply ND '()) 'accept)
(check-equal? (sm-apply ND '(a b)) 'accept)
(check-equal? (sm-apply ND '(a b a)) 'accept)
(check-equal? (sm-apply ND '(a b a a b a b)) 'accept)

;; L = (aba)* U (ab)*
(define D
  (make-dfa `(S A B C ,DEAD)
            '(a b)
            'S
            '(S B C)
            `((S a A)
              (S b ,DEAD)
              (A a ,DEAD)
              (A b B)
              (B a C)
              (B b ,DEAD)
              (C a A)
              (C b B)
              (,DEAD a ,DEAD)
              (,DEAD b ,DEAD))))

;; Tests for D
(check-equal? (sm-testequiv? D ND 500) #t)
(check-equal? (sm-testequiv? (ndfa->dfa ND) D 500) #t)
