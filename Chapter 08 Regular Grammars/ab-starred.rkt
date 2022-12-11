#lang fsm

;; L {w | w in (a b)*}
(define RG (make-rg '(S A)
                    '(a b)
                    `((S ,ARROW ,EMP)
                      (S ,ARROW aA)
                      (A ,ARROW bS))
                    'S))

;; word --> Boolean
(define (in-RG? w)
  (in-S? w))

(define (in-S? w)
  (or (empty? w)
      (and (eq? (first w) 'a)
           (in-A? (rest w)))))

(define (in-A? w)
  (and (not (empty? w))
       (eq? (first w) 'b)
       (in-S? (rest w))))

;; Tests for in-RG?
(check-equal? (in-RG? '(a b a a b)) #f)
(check-equal? (in-RG? '()) #t)
(check-equal? (in-RG? '(a b a b a b)) #t)