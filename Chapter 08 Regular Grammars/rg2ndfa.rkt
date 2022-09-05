#lang racket

(require fsm rackunit "dfa2rg.rkt")

(define EVEN-A-ODD-B (make-dfa '(S M N P)
                               '(a b)
                               'S
                               '(N)
                               '((S a P)
                                 (S b N)
                                 (M a N)
                                 (M b P)
                                 (N a M)
                                 (N b S)
                                 (P a S)
                                 (P b M))
                               'no-dead))

(define SIGMA* (make-dfa '(S)
                         '(a b c)
                         'S
                         '(S)
                         '((S a S)
                           (S b S)
                           (S c S))
                         'no-dead))

;; Sample rg
;; L = (a U b U c)*
(define SIGMA*-rg (dfa2rg SIGMA*))

;; L = {w | w has an even number of a and an odd number of b}
(define EA-OB-rg (dfa2rg EVEN-A-ODD-B))

;; L = a* U b*
(define a*Ub*-rg (make-rg '(S A B)
                          '(a b)
                          `((S ,ARROW ,EMP)
                            (S ,ARROW aA)
                            (S ,ARROW bB)
                            (S ,ARROW a)
                            (S ,ARROW b)
                            (A ,ARROW aA)
                            (A ,ARROW a)
                            (B ,ARROW bB)
                            (B ,ARROW b))
                          'S))
;; L = {a aba}
(define a-aba-rg (make-rg '(S A B)
                          '(a b)
                          `((S ,ARROW a)
                            (S ,ARROW aA)
                            (A ,ARROW bB)
                            (B ,ARROW a))
                          'S))


;; rg --> ndfa
;; Purpose: Build a ndfa for the language of the given regular grammar
(define (rg2ndfa rg)
  (let* [(final-state (generate-symbol 'Z (grammar-nts rg)))
         (states (cons final-state (grammar-nts rg)))
         (sigma (grammar-sigma rg))
         (start (grammar-start rg))
         (finals (list final-state))
         (simple-prs
          (filter 
           (λ (pr)
             (= (length (symbol->fsmlos (third pr))) 1))
           (grammar-rules rg)))
         (cmpnd-prs
          (filter
           (λ (pr)
             (= (length (symbol->fsmlos (third pr))) 2))
           (grammar-rules rg)))
         (rules (append
                 (map (λ (spr)
                        (list (first spr) (third spr) final-state))
                      simple-prs)
                 (map (λ (pr)
                        (let [(rhs (symbol->fsmlos (third pr)))]
                          (list (first pr) (first rhs) (second rhs))))
                      cmpnd-prs)))]
    (make-ndfa states sigma start finals rules)))

;; Tests for rg2ndfa
(define SIGMA*2 (rg2ndfa SIGMA*-rg))
(define EA-OB (rg2ndfa EA-OB-rg))
(define a*Ub* (rg2ndfa a*Ub*-rg))
(define a-aba (rg2ndfa a-aba-rg))


(check-equal? (eq? (last (grammar-derive SIGMA*-rg '())) EMP)
              (eq? (sm-apply SIGMA*2 '()) 'accept))
(check-equal? (eq? (last (grammar-derive SIGMA*-rg '(a b c)))
                   (los->symbol '(a b c)))
              (eq? (sm-apply SIGMA*2 '(a b c)) 'accept))
(check-equal? (eq? (last (grammar-derive SIGMA*-rg '(c c a b a c)))
                   (los->symbol '(c c a b a c)))
              (eq? (sm-apply SIGMA*2 '(c c a b a c)) 'accept))
(check-equal? (sm-testequiv? SIGMA* SIGMA*2) #t)

(check-equal? (string=? (grammar-derive EA-OB-rg '(a b))
                        "(a b) is not in L(G).")
              (eq? (sm-apply EA-OB '(a b)) 'reject))
(check-equal? (string=? (grammar-derive EA-OB-rg '(a a b a))
                        "(a a b a) is not in L(G).")
              (eq? (sm-apply EA-OB '(a a b a)) 'reject))
(check-equal? (eq? (last (grammar-derive EA-OB-rg '(b)))
                   (los->symbol '(b)))
              (eq? (sm-apply EA-OB '(b)) 'accept))
(check-equal? (eq? (last (grammar-derive EA-OB-rg '(b a a b b)))
                   (los->symbol '(b a a b b)))
              (eq? (sm-apply EA-OB '(b a a b b)) 'accept))
(check-equal? (sm-testequiv? EVEN-A-ODD-B EA-OB) #t)

(check-equal? (eq? (sm-apply a*Ub* '(a b)) 'reject)
              (string=? (grammar-derive a*Ub*-rg '(a b))
                        "(a b) is not in L(G)."))
(check-equal? (eq? (sm-apply a*Ub* '(a b b a)) 'reject)
              (string=? (grammar-derive a*Ub*-rg '(a b b a))
                        "(a b b a) is not in L(G)."))
(check-equal? (eq? (sm-apply a*Ub* '()) 'accept)
              (eq? (last (grammar-derive a*Ub*-rg '()))
                   EMP))
(check-equal? (eq? (sm-apply a*Ub* '(a a a)) 'accept)
              (eq? (last (grammar-derive a*Ub*-rg '(a a a)))
                   'aaa))
(check-equal? (eq? (sm-apply a*Ub* '(b)) 'accept)
              (eq? (last (grammar-derive a*Ub*-rg '(b)))
                   'b))

(check-equal? (eq? (sm-apply a-aba '(b b)) 'reject)
              (string=? (grammar-derive a-aba-rg '(b b))
                        "(b b) is not in L(G)."))
(check-equal? (eq? (sm-apply a-aba '()) 'reject)
              (string=? (grammar-derive a-aba-rg '())
                        "() is not in L(G)."))
(check-equal? (eq? (sm-apply a-aba '(a b a)) 'accept)
              (eq? (last (grammar-derive a-aba-rg '(a b a)))
                   'aba))
(check-equal? (eq? (sm-apply a-aba '(a)) 'accept)
              (eq? (last (grammar-derive a-aba-rg '(a)))
                   'a))

(define G (make-rg '(S A B C D)
                   '(a b)
                   `((S ,ARROW ,EMP)
                     (S ,ARROW aA)
                     (S ,ARROW bB)
                     (S ,ARROW a)
                     (S ,ARROW b)
                     (A ,ARROW aA)
                     (A ,ARROW a)
                     (B ,ARROW bB)
                     (B ,ARROW b)
                     (C ,ARROW aD)
                     (D ,ARROW aD)
                     (D ,ARROW bD))
                   'S))

(define M (rg2ndfa G))

