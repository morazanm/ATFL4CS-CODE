#lang fsm

;; L = {ε} U ba*
(define EMP-U-ba* (make-rg '(S A)
                           '(a b)
                           `((S ,ARROW ,EMP)
                             (S ,ARROW b)
                             (S ,ARROW bA)
                             (A ,ARROW a)
                             (A ,ARROW aA))
                           'S))

;; Tests for EMP-U-ba*
(check-equal? (grammar-derive EMP-U-ba* '(a))
              "(a) is not in L(G).")
(check-equal? (grammar-derive EMP-U-ba* '(a b b a a))
              "(a b b a a) is not in L(G).")
(check-equal? (grammar-derive EMP-U-ba* '())
              '(S -> ε))
(check-equal? (grammar-derive EMP-U-ba* '(b a))
              '(S -> bA -> ba))
(check-equal? (grammar-derive EMP-U-ba* '(b a a a))
              '(S -> bA -> baA -> baaA -> baaa))