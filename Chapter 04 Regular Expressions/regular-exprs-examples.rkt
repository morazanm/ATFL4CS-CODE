#lang fsm

(define A (singleton-regexp "a"))
(define B (singleton-regexp "b"))
(define AUB (union-regexp A B))
(define AUB* (kleenestar-regexp AUB))
  
(define ENDS-WITH-A  (concat-regexp AUB* A))

(check-equal? (printable-regexp ENDS-WITH-A) "(a U b)*a")


(define ZERO (singleton-regexp "0"))

(define ONE  (singleton-regexp "1"))

(define 0U1* (kleenestar-regexp (union-regexp ZERO ONE)))

(define STARTS1 (concat-regexp ONE 0U1*))

(define BIN-NUMS (union-regexp ZERO STARTS1))

(check-equal? (printable-regexp BIN-NUMS) "(0 U 1(0 U 1)*)")