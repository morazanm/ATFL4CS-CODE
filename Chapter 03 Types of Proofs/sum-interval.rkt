#lang fsm

;; natnum --> natnum
;; Purpose: Compute the sum of the integers in the given interval
(define (sum-interval low high)
  ;; integer integer \arrow integer
  ;; Purpose: Compute the sum of the integers in [low..k]
  ;; Accumulator Invariant
  ;;    acc = SUM i for i=k+1 to high
  (define (helper k acc)
    (if (< k low)
        acc
        (helper (sub1 k) (+ k acc))))
  (helper high 0))
           
(check-equal? (sum-interval 5 4)  0)
(check-equal? (sum-interval 1 3)  6)
(check-equal? (sum-interval -4 2) -7)
