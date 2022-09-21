#lang racket

(require fsm rackunit)

;; Syntactic Categories
;;  S = words that start with n a and end with n b

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))

;; Tests for a2nb2n
(check-equal? (grammar-derive a2nb2n '(b b b))
              "(b b b) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b a))
              "(a b a) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b))
              '(S -> aSb -> ab))
(check-equal? (grammar-derive a2nb2n '(a a a b b b))
              '(S -> aSb -> aaSbb -> aaaSbbb -> aaabbb))

;;(grammar-test a2nb2n 10)