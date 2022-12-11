#lang racket

(require fsm rackunit)

(define X 220)

(define Y -10)

;; number number --> number
;; Purpose: Compute f(a, b) = 2a + b
(define (f a b) (+ (* 2 a) b))

(check-equal? (f 0 0) 0)
(check-equal? (f X Y) 430)