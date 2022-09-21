#lang racket

(require fsm rackunit)

;; L = {w | w has an even number of and an odd number of b}

;; States
;; S: even number of a and even number of b, start state
;; M: odd number of a and odd number of b
;; N: even number of a and odd number of b, final state
;; P: odd number of a and even number of b

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

;; Tests for EVEN-A-ODD-B
(check-equal? (sm-apply EVEN-A-ODD-B '()) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b b a)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b a b b a a)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b b b b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b a b b a a b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b)) 'accept)
(check-equal? (sm-apply EVEN-A-ODD-B '(a a b)) 'accept)
(check-equal? (sm-apply EVEN-A-ODD-B '(a a a b a b b)) 'accept)


;; word --> Boolean
;; Purpose: Determine if given word has an even number of a
;;          and an odd number of b
(define (S-INV ci)
  (and (even? (length (filter (λ (s) (eq? s 'a)) ci)))
       (even? (length (filter (λ (s) (eq? s 'b)) ci)))))

;; Tests for S-INV
(check-equal? (S-INV '(a)) #f)
(check-equal? (S-INV '(a b b b a)) #f)
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a a b b)) #t)


;; word --> Boolean
;; Purpose: Determine if given word has an odd number of a
;;          and an odd number of b
(define (M-INV ci)
  (and (odd? (length (filter (λ (s) (eq? s 'a)) ci)))
       (odd? (length (filter (λ (s) (eq? s 'b)) ci)))))

;; Tests for S-INV
(check-equal? (M-INV '(a)) #f)
(check-equal? (M-INV '(a b b b a)) #f)
(check-equal? (M-INV '(a b b b a a b)) #f)
(check-equal? (M-INV '(b a)) #t)
(check-equal? (M-INV '(b a a b a b)) #t)


;; word --> Boolean
;; Purpose: Determine if given word has an even number of a
;;          and an odd number of b
(define (N-INV ci)
  (and (even? (length (filter (λ (s) (eq? s 'a)) ci)))
       (odd? (length (filter (λ (s) (eq? s 'b)) ci)))))

;; Tests for N-INV
(check-equal? (N-INV '()) #f)
(check-equal? (N-INV '(a b a b a)) #f)
(check-equal? (N-INV '(a b b a a b)) #f)
(check-equal? (N-INV '(b a a)) #t)
(check-equal? (N-INV '(a b a a b a b b b)) #t)


;; word --> Boolean
;; Purpose: Determine if given word has an odd number of a
;;          and an even number of b
(define (P-INV ci)
  (and (odd? (length (filter (λ (s) (eq? s 'a)) ci)))
       (even? (length (filter (λ (s) (eq? s 'b)) ci)))))

;; Tests for P-INV
(check-equal? (P-INV '()) #f)
(check-equal? (P-INV '(a b)) #f)
(check-equal? (P-INV '(a b b a a b a)) #f)
(check-equal? (P-INV '(b a b)) #t)
(check-equal? (P-INV '(a b a a b b b)) #t)




