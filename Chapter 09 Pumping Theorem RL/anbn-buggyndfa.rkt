#lang racket

(require fsm rackunit)

(define a2n-b2n (make-ndfa '(S F)
                          '(a b)
                          'S
                          '(F)
                          `((S a S)
                            (S ,EMP F)
                            (F b F))))

;; Tests for a2n-b2n
(check-equal? (sm-apply a2n-b2n '(b b a a)) 'reject)
(check-equal? (sm-apply a2n-b2n '()) 'accept)
(check-equal? (sm-apply a2n-b2n '(a b)) 'accept)
(check-equal? (sm-apply a2n-b2n '(a a b b)) 'accept)
(check-equal? (sm-apply a2n-b2n '(a a a b b b)) 'accept)

;(check-equal? (sm-apply a2n-b2n '(a a)) 'reject)
;(check-equal? (sm-apply a2n-b2n '(b)) 'reject)
;(check-equal? (sm-apply a2n-b2n '(a a a b)) 'reject)