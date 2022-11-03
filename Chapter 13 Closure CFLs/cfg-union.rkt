#lang racket

(require fsm rackunit)

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
;; Purpose: Construct a grammar for the union of the given grammars
(define (cfg-union G1 G2)
  (let* [(H (grammar-rename-nts (grammar-nts G1) G2))
         (G-nts (grammar-nts G1))     (H-nts (grammar-nts H))
         (G-sigma (grammar-sigma G1)) (H-sigma (grammar-sigma H))
         (G-rules (grammar-rules G1)) (H-rules (grammar-rules H))
         (G-start (grammar-start G1)) (H-start (grammar-start H))
         (A (generate-symbol 'S (append G-nts H-nts)))]
    (make-cfg (cons A (append G-nts H-nts))
              (remove-duplicates (append G-sigma H-sigma))
              (append (list (list A ARROW G-start)
                            (list A ARROW H-start))
                      G-rules
                      H-rules)
              A)))

(define a2nb2nUnumb>numa (cfg-union a2nb2n numb>numa))
(define numb>numaUMULT3-as (cfg-union numb>numa MULT3-as))

(check-equal? (grammar-derive a2nb2nUnumb>numa '(b a))
              "(b a) is not in L(G).")
(check-equal? (grammar-derive a2nb2nUnumb>numa '(b a a a b b))
              "(b a a a b b) is not in L(G).")
(check-equal? (last (grammar-derive a2nb2nUnumb>numa '(a b)))
              'ab)
(check-equal? (last (grammar-derive a2nb2nUnumb>numa '(a a b b)))
              'aabb)
(check-equal? (last (grammar-derive a2nb2nUnumb>numa '(b b)))
              'bb)
(check-equal? (last (grammar-derive a2nb2nUnumb>numa '(b a a b b)))
              'baabb)

(check-equal? (grammar-derive numb>numaUMULT3-as '(a a a a b b))
              "(a a a a b b) is not in L(G).")
(check-equal? (grammar-derive numb>numaUMULT3-as '(a a))
              "(a a) is not in L(G).")
(check-equal? (last (grammar-derive numb>numaUMULT3-as '(b b b)))
              'bbb)
(check-equal? (last (grammar-derive numb>numaUMULT3-as '(a b a b a b)))
              'ababab)



