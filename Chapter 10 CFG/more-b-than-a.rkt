#lang racket

(require fsm rackunit)

;; Syntactic Categories
;;  S = words that number of b > number of a
;;  A = words that number of b >= number of a

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

; Tests for numb>numa
(check-equal? (grammar-derive numb>numa '(a b))
              "(a b) is not in L(G)")
(check-equal? (grammar-derive numb>numa '(a b a))
              "(a b a) is not in L(G)")
(check-equal? (grammar-derive numb>numa '(a a a a a))
              "(a a a a a) is not in L(G)")
(check-equal? (grammar-derive numb>numa '(b b b))
              '(S -> AbA -> bA -> bbA -> bbbA -> bbb))
(check-equal? (grammar-derive numb>numa '(b b a b a a b))
              '(S -> AbA -> AbAaAbA -> bAaAbA -> bAbAaAaAbA
                  -> bAbAaAbAaAaAbA -> bbAaAbAaAaAbA -> bbaAbAaAaAbA
                  -> bbabAaAaAbA -> bbabaAaAbA -> bbabaaAbA
                  -> bbabaabA -> bbabaab))
(check-equal? (grammar-derive numb>numa '(a a a b b b b))
              '(S -> AbA -> AaAbAbA -> aAbAbA -> aAaAbAbAbA
                  -> aaAbAbAbA -> aaAaAbAbAbAbA -> aaaAbAbAbAbA
                  -> aaabAbAbAbA -> aaabbAbAbA -> aaabbbAbA
                  ->  aaabbbbA -> aaabbbb))

;(grammar-test numb>numa 10)


