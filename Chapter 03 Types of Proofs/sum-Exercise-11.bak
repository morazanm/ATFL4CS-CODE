#lang racket

(require fsm rackunit)

;; natnum \arrow natnum
;; Purpose: Compute the sum of the first n natural numbers
(define (sum-natnums n)
  (if (= n 0)
      0
      (+ n (sum-natnums (sub1 n)))))
           
(check-equal? (sum-natnums 0)  0)
(check-equal? (sum-natnums 5)  15)
(check-equal? (sum-natnums 20) 210)