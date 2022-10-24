#lang racket

(require fsm rackunit)

;; Sample ndfa

;; L = {w | a not in w OR b not in w OR c not in w}
(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C)
                                        '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))

;; L = {ε} U aa* U ab*
(define LNDFA (make-ndfa '(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A b A)
                           (B a B))))

;; ndfa --> pda
;; Purpose: Convert the given ndfa to a pda
(define (ndfa->pda M)
  (let [(states (sm-states M))
        (sigma  (sm-sigma M))
        (start  (sm-start M))
        (finals (sm-finals M))
        (rules  (sm-rules M))]
    (make-ndpda states
                sigma
                '()
                start
                finals
                (map (λ (r) (list (list (first r) (second r) EMP)
                                  (list (third r) EMP)))
                     rules))))

;; Sample pda
(define ALOM-PDA  (ndfa->pda AT-LEAST-ONE-MISSING))
(define LNDFA-PDA (ndfa->pda LNDFA)) 

(check-equal? (sm-testequiv? ALOM-PDA AT-LEAST-ONE-MISSING) #t)
(check-equal? (sm-testequiv? LNDFA-PDA LNDFA) #t)

