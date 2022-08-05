#lang racket

(require fsm rackunit)

;; natnum \arrow natnum
;; Purpose: Compute the nth Fibonacci number
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (sub1 n)) (fib (- n 2)))))
           
(check-equal? (fib 0)  0)
(check-equal? (fib 1)  1)
(check-equal? (fib 5)  5)
(check-equal? (fib 10) 55)