#lang fsm

;; Sample ndfa
;; L = (a U b U c)*
(define SIGMA* (make-dfa '(S)
                         '(a b c)
                         'S
                         '(S)
                         '((S a S)
                           (S b S)
                           (S c S))
                         'no-dead))

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

;; (listof dfa-rule) --> (listof rg-rule)
;; Purpose: Generate the production rules for the given transition rules
(define (mk-prod-rules mrules mfinals)
  (append-map (λ (r)
                (if (not (member (third r) mfinals))
                    (list (list (first r) ARROW (los->symbol (rest r))))
                    (list (list (first r) ARROW (second r))
                          (list (first r) ARROW (los->symbol (rest r))))))
              mrules))

;; Tests for mk-prod-rules
(check-equal? (mk-prod-rules '() '(F G)) '())
(check-equal? (mk-prod-rules '((S a F)
                               (S b R)
                               (R a G)
                               (R b R)
                               (G a G)
                               (G b G))
                             '(F G))
              '((S -> a) (S -> aF) (S -> bR)
                (R -> a) (R -> aG) (R -> bR)
                (G -> a) (G -> aG) (G -> b) (G -> bG)))

;; dfa --> rg
;; Purpose: Build a rg for the language of the given dfa
;; Assume: All states in the given dfa are represented by a single capital letter in the Roman alphabet
(define (dfa2rg m)
  (let* [(nts (sm-states m))
         (sigma (sm-sigma m))
         (startnt (sm-start m))
         (prules (if (member (sm-start m) (sm-finals m))
                     (cons (list (sm-start m) ARROW EMP)
                           (mk-prod-rules (sm-rules m) (sm-finals m)))
                     (mk-prod-rules (sm-rules m) (sm-finals m))))]
    (make-rg nts sigma prules startnt)))

;; Tests for dfa2rg
(define SIGMA*-rg (dfa2rg SIGMA*))
(define EA-OB-rg (dfa2rg EVEN-A-ODD-B))

(check-equal? (eq? (last (grammar-derive SIGMA*-rg '())) EMP)
              (eq? (sm-apply SIGMA* '()) 'accept))
(check-equal? (eq? (last (grammar-derive SIGMA*-rg '(a b c)))
                   (los->symbol '(a b c)))
              (eq? (sm-apply SIGMA* '(a b c)) 'accept))
(check-equal? (eq? (last (grammar-derive SIGMA*-rg '(c c a b a c)))
                   (los->symbol '(c c a b a c)))
              (eq? (sm-apply SIGMA* '(c c a b a c)) 'accept))

(check-equal? (string? (grammar-derive EA-OB-rg '(a b)))
              (eq? (sm-apply EVEN-A-ODD-B '(a b)) 'reject))
(check-equal? (string? (grammar-derive EA-OB-rg '(a a b a)))
              (eq? (sm-apply EVEN-A-ODD-B '(a a b a)) 'reject))
(check-equal? (eq? (last (grammar-derive EA-OB-rg '(b)))
                   (los->symbol '(b)))
              (eq? (sm-apply EVEN-A-ODD-B '(b)) 'accept))
(check-equal? (eq? (last (grammar-derive EA-OB-rg '(b a a b b)))
                   (los->symbol '(b a a b b)))
              (eq? (sm-apply EVEN-A-ODD-B '(b a a b b)) 'accept))

