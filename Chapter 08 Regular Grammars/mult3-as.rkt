#lang fsm

;; L = {w | the number of as in w is a multiple of 3}

;; Syntactic categories
;; S = words with 3n as
;; B = words with 3n + 2 as
;; C = words with 3n + 1 as

(define MULT3-as (make-rg '(S B C)
                          '(a b)
                          `((S ,ARROW ,EMP)
                            (S ,ARROW aB)
                            (S ,ARROW bS)
                            (B ,ARROW aC)
                            (B ,ARROW bB)
                            (C ,ARROW aS)
                            (C ,ARROW bC))
                          'S))

;; Tests for MULT3-as
(check-equal? (grammar-derive MULT3-as '(b b a b b))
              "(b b a b b) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '(b b a b b a))
              "(b b a b b a) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '(b b a b a b a a b))
              "(b b a b a b a a b) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '())
              `(S ,ARROW ,EMP))
(check-equal? (grammar-derive MULT3-as '(a a a))
              `(S ,ARROW aB ,ARROW aaC ,ARROW aaaS ,ARROW aaa))
(check-equal? (grammar-derive MULT3-as '(b b a a b a b b))
              `(S ,ARROW bS ,ARROW bbS ,ARROW bbaB ,ARROW bbaaC ,ARROW bbaabC ,ARROW
                bbaabaS ,ARROW bbaababS ,ARROW bbaababbS ,ARROW  bbaababb))