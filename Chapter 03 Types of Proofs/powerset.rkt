#lang racket

(require fsm test-engine/racket-tests)

#|
 Data Definitions
   A list of X, lox, is either:
     1. '()
     2. (cons X lox)

   A set of X, setx, is a a (listof X)
|#

;; Sample setx
(define EMPTY-SET '())
(define SET1 '(r e a))

;; setx --> (listof setx)
;; Purpose: Return the power set of the given set
(define (powerSet A)
  (cond [(null? A) (list '())]
        [else 
         (let ((rest (powerSet (cdr A))))
           (append 
            (map (lambda (x) (cons (car A) x)) rest)
            rest))]))

(check-expect (powerSet EMPTY-SET) '(()))
(check-expect (powerSet SET1) '((r e a) (r e) (r a) (r) (e a) (e) (a) ()))

(test)