#lang racket

(require fsm rackunit)

;; X (listof X) \arrow number
;; Purpose: Count the number of times the given value 
;;          occurs in the given list
(define (count-occurences x L)
  (foldl (λ (an-x acc)
           (if (equal? x an-x) (add1 acc) acc))
         0
         L))

(check-equal? (count-occurences "hi" '()) 0)
(check-equal? (count-occurences 'x '(a b x g x h)) 2)
(check-equal? (count-occurences '(1 2) 
                                '((w q) (9 -3) (1 2))) 
              1)
(check-equal? (count-occurences #f '(#t #t #t)) 0)