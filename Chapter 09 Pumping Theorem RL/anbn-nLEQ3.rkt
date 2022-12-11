#lang fsm

(define a^<=3b^<=3 (make-ndfa '(S A B C D E F)
                              '(a b)
                              'S
                              '(F)
                              `((S a A)
                                (S ,EMP F)
                                (A a B)
                                (A ,EMP E)
                                (B a C)
                                (B ,EMP D)
                                (C b D)
                                (D b E)
                                (E b F))))

;; Tests for a^<=3b^<=3
(check-equal? (sm-apply a^<=3b^<=3 '(a a)) 'reject)
(check-equal? (sm-apply a^<=3b^<=3 '(b b a a)) 'reject)
(check-equal? (sm-apply a^<=3b^<=3 '()) 'accept)
(check-equal? (sm-apply a^<=3b^<=3 '(a b)) 'accept)
(check-equal? (sm-apply a^<=3b^<=3 '(a a b b)) 'accept)
(check-equal? (sm-apply a^<=3b^<=3 '(a a a b b b)) 'accept)