#lang racket

(require fsm rackunit)

;; X (listof X) --> number
;; Purpose: Count the number of times the given value occurs in the given list
(define (count-occurences x L)
  (cond [(empty? L) 0]
        [(equal? x (first L))
         (add1 (count-occurences x (rest L)))]
        [else (count-occurences x (rest L))]))

(check-equal? (count-occurences "hi" '()) 0)
(check-equal? (count-occurences 'x '(a b x g x h)) 2)
(check-equal? (count-occurences '(1 2) '((w q) (9 -3) (1 2))) 1)
(check-equal? (count-occurences #f '(#t #t #t)) 0)