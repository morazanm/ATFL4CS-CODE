#lang fsm

;; natnum --> natnum
;; Purpose: Compute n!
(define (fact n)
  ;; natnum natnum --> natnum
  ;; Purpose: Compute the factorial of the given natural number
  ;; Accumulator Invariant
  ;;    accum = PI i, for i in [j+1..n]
  (define (helper j accum)
    (if (= j 0)
        accum
        (helper (sub1 j) (* j accum))))
  (helper n 1))

(check-equal? (fact 0)  1)
(check-equal? (fact 5)  120)
(check-equal? (fact 10) 3628800)