#lang racket

(require fsm rackunit)

;; States
;; S the consumed input is empty, starting state
;; A the consumed input does not contain a, final state
;; B the consumed input does not contain b, final state
;; C the consumed input does not contain c, final state

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

;; Tests for AT-LEAST-ONE-MISSING
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a b c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b b a b c b a)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b a c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '()) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(c)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(c c a a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b b c b b b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a a a b b b)) 'accept)

;; States
;; S the consumed input is empty, starting state
;; A the consumed input does not contain a, final state
;; B the consumed input does not contain b, final state
;; C the consumed input does not contain c, final state

;; word --> Boolean
;; Purpose: Determine if the given word is empty
(define (S-INV ci) (empty? ci))

;; Test for S-INV
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a b)) #f)

;; word --> Boolean
;; Purpose: Determine if the given word does not contain a
(define (A-INV ci) (empty? (filter (λ (a) (eq? a 'a)) ci)))

;; Test for A-INV
(check-equal? (A-INV '(a)) #f)
(check-equal? (A-INV '(a c b)) #f)
(check-equal? (A-INV '(c c b a b)) #f)
(check-equal? (A-INV '(b)) #t)
(check-equal? (A-INV '(c c b c b)) #t)
(check-equal? (A-INV '()) #t)

;; word --> Boolean
;; Purpose: Determine if the given word does not contain b
(define (B-INV ci) (empty? (filter (λ (a) (eq? a 'b)) ci)))

;; Test for B-INV
(check-equal? (B-INV '(b)) #f)
(check-equal? (B-INV '(a c b)) #f)
(check-equal? (B-INV '(a a b a b)) #f)
(check-equal? (B-INV '(c)) #t)
(check-equal? (B-INV '(c c a c c a a a)) #t)
(check-equal? (B-INV '()) #t)

;; word --> Boolean
;; Purpose: Determine if the given word does not contain c
(define (C-INV ci) (empty? (filter (λ (a) (eq? a 'c)) ci)))

;; Test for C-INV
(check-equal? (C-INV '(c)) #f)
(check-equal? (C-INV '(a b c b)) #f)
(check-equal? (C-INV '(c c b a b)) #f)
(check-equal? (C-INV '(b)) #t)
(check-equal? (C-INV '(b b a a b a a a)) #t)
(check-equal? (C-INV '()) #t)

(sm-visualize AT-LEAST-ONE-MISSING (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'C C-INV))


