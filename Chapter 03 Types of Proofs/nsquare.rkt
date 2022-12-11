#lang fsm

;; natnum --> natnum
;; Purpose: Compute the square of the given natnum
(define (square n)
  (if (= n 0)
      0
      (+ (sub1 (* 2 n)) (square (sub1 n)))))

;; Tests
(check-equal? (square 0)   0)
(check-equal? (square 5)   25)
(check-equal? (square 100) 10000)