#lang fsm

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))

;; L = {w | w in (a b)* AND  w has more b than a}
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; L = {w | number of a in w is a multiple of 3}
(define MULT3-as (make-cfg '(S B C)
                           '(a b)
                           `((S ,ARROW ,EMP)
                             (S ,ARROW aB)
                             (S ,ARROW bS)
                             (B ,ARROW aC)
                             (B ,ARROW bB)
                             (C ,ARROW aS)
                             (C ,ARROW bC))
                           'S))

;; cfg cfg --> cfg
;; Purpose: Return a cfg for the concatenation of the languages of the given cfgs
(define (cfg-concat G1 G2)
  (let* [(G2 (grammar-rename-nts (grammar-nts G1) G2))
         (G1-nts (grammar-nts G1)) (G2-nts (grammar-nts G2))
         (G1-sigma (grammar-sigma G1)) (G2-sigma (grammar-sigma G2))
         (G1-rules (grammar-rules G1)) (G2-rules (grammar-rules G2))
         (G1-start (grammar-start G1)) (G2-start (grammar-start G2))
         (A (generate-symbol 'S (append G1-nts G2-nts)))]
    (make-cfg (cons A (append G1-nts G2-nts))
              (remove-duplicates (append G1-sigma G2-sigma))
              (append (list (list A ARROW (los->symbol (list G1-start G2-start))))
                      G1-rules
                      G2-rules)
              A)))

;; Tests for cfg-concat

(define a2nb2n-numb>numa (cfg-concat a2nb2n numb>numa))
(define MULT3-as-a2nb2n (cfg-concat MULT3-as a2nb2n))

(check-equal? (grammar-derive a2nb2n-numb>numa '(b a))
              "(b a) is not in L(G).")
(check-equal? (grammar-derive a2nb2n-numb>numa '(a a a))
              "(a a a) is not in L(G).")
(check-equal? (grammar-derive a2nb2n-numb>numa '(a a b b))
              "(a a b b) is not in L(G).")
(check-equal? (last (grammar-derive a2nb2n-numb>numa '(b b b)))
              'bbb)
(check-equal? (last (grammar-derive a2nb2n-numb>numa '(a b b)))
              'abb)
(check-equal? (last (grammar-derive a2nb2n-numb>numa '(a a b b b b b)))
              'aabbbbb)

(check-equal? (grammar-derive MULT3-as-a2nb2n '(a a))
              "(a a) is not in L(G).")
(check-equal? (grammar-derive MULT3-as-a2nb2n '(b b b b a))
              "(b b b b a) is not in L(G).")
(check-equal? (last (grammar-derive MULT3-as-a2nb2n '(a a b b)))
              'aabb)
(check-equal? (last (grammar-derive MULT3-as-a2nb2n '(b b b b b b)))
              'bbbbbb)


