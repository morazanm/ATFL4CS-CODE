#lang racket

(require fsm rackunit)

;; L = {w | w does not contains abba}
;; States
;;  S: nothing detected, start state
;;  A: a    has been detected, final state
;;  B: ab   has been detected, final state
;;  C: aba  has been detected, final state
;;  R: abaa has been detected

(define NO-ABAA (make-dfa '(S A B C R)
                          '(a b)
                          'S
                          '(S A B C)
                          '((S a A)
                            (S b S)
                            (A a A)
                            (A b B)
                            (B a C)
                            (B b S)
                            (C a R)
                            (C b B)
                            (R a R)
                            (R b R))
                          'no-dead))

;; Tests
(check-equal? (sm-apply NO-ABAA '()) 'accept)
(check-equal? (sm-apply NO-ABAA '(a a b a b b)) 'accept)
(check-equal? (sm-apply NO-ABAA '(a b b a a a b b b a b b a a b)) 'accept)
(check-equal? (sm-apply NO-ABAA '(a b a a)) 'reject)
(check-equal? (sm-apply NO-ABAA '(a b a b a a b a)) 'reject)
(check-equal? (sm-apply NO-ABAA '(a b a b b b a a b a a b b)) 'reject)

;; State Invariants
(define PROHIBITED-PATTERN '(a b a a))

;; word word --> Boolean
;; Purpose: Determine if the second given word appears in the first given word
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))

;; Tests for contains?
(check-equal? (contains? '() PROHIBITED-PATTERN) #f)
(check-equal? (contains? '(a b b a a) PROHIBITED-PATTERN) #f)
(check-equal? (contains? '(b b b a b a b b a b a) PROHIBITED-PATTERN) #f)
(check-equal? (contains? '(a b a a) PROHIBITED-PATTERN) #t)
(check-equal? (contains? '(a b b b a b a a a) PROHIBITED-PATTERN) #t)
(check-equal? (contains? '(a b a b a a a b a a b) PROHIBITED-PATTERN) #t)

;; word --> Boolean
;; Purpose: Determine if the consumed input does not contains PROHIBITED-PATTERN
(define (S-INV ci)
  (or (= (length ci) 0)
      (and (not (contains? ci PROHIBITED-PATTERN))
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (not (equal? (drop ci (- (length ci) 2)) '(a b)))))))
  
;; word --> Boolean
;; Purpose: Determine if the consumed input minus 
;;          the last symbol does not contains 
;;          PROHIBITED-PATTERN  and the last symbol 
;;          of the consumed input is a
;; Assume: length of given word > 0
(define (A-INV ci)
  (and (equal? (drop ci (sub1 (length ci))) '(a))
       (not (contains? ci PROHIBITED-PATTERN))
       (or (< (length ci) 3)
           (not (equal? (drop ci (- (length ci) 3)) '(a b a))))))
           

;; word --> Boolean
;; Purpose: Determine if the last two symbols of the consumed input are a b
;;          and the consumed input minus the last two symbols does not contains PROHIBITED-PATTERN
;; Assume: length of given word >= 2
(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 2)) '(a b)) 
       (not (contains? ci PROHIBITED-PATTERN))))

;; word --> Boolean
;; Purpose: Determine if the last three symbols of the consumed input are a b
;;          and the consumed input minus the last three symbols does not contains PROHIBITED-PATTERN
;; Assume: length of given word >= 3
(define (C-INV ci)
  (and (equal? (drop ci (- (length ci) 3)) '(a b a))
       (not (contains? ci PROHIBITED-PATTERN))))

;; word --> Boolean
;; Purpose: Determine if the consumed input does contains PROHIBITED-PATTERN
(define (R-INV ci)
  (contains? ci PROHIBITED-PATTERN))

;; Tests for S-INV
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(b)) #t)
(check-equal? (S-INV '(b b)) #t)
(check-equal? (S-INV '(a b b)) #t)
(check-equal? (S-INV '(b a b b a b a b b)) #t)
(check-equal? (S-INV '(a)) #f)
(check-equal? (S-INV '(a b)) #f)
(check-equal? (S-INV '(a b a)) #f)
(check-equal? (S-INV '(a b a a)) #f)
(check-equal? (S-INV '(a b b b b a b a a b b)) #f)
(check-equal? (S-INV '(a a b b b b a b a b b a b a)) #f)

;; Tests for A-INV
(check-equal? (A-INV '(b)) #f)
(check-equal? (A-INV '(a b)) #f)
(check-equal? (A-INV '(a b a)) #f)
(check-equal? (A-INV '(a b a a b a a b)) #f)
(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(a a)) #t)
(check-equal? (A-INV '(b b a)) #t)
(check-equal? (A-INV '(a b b a b a b b a)) #t)

;; Tests for B-INV
(check-equal? (B-INV '(a b)) #t)
(check-equal? (B-INV '(a a b)) #t)
(check-equal? (B-INV '(a b b b a b a b)) #t)
(check-equal? (B-INV '(a a b a a)) #f)
(check-equal? (B-INV '(a b a)) #f)
(check-equal? (B-INV '(a b a b a b a)) #f)
(check-equal? (B-INV '(a b b b a b a a b b a)) #f)

;; Tests for C-INV
(check-equal? (C-INV '(a b a)) #t)
(check-equal? (C-INV '(a b a b b a a b a b a)) #t)
(check-equal? (C-INV '(a b b)) #f)
(check-equal? (C-INV '(a b b a b a a b a)) #f)

(check-equal? (R-INV '(a)) #f)
(check-equal? (R-INV '(a b)) #f)
(check-equal? (R-INV '(a b a)) #f)
(check-equal? (R-INV '(a b a a)) #t)
(check-equal? (R-INV '(a a b b a b a a b b a)) #t)

;(sm-visualize NO-ABAA (list 'S S-INV) (list 'A A-INV) (list 'B B-INV)
;                      (list 'C C-INV) (list 'R RINV))

;(sm-visualize NO-ABAA (list 'S S-INV)  (list 'R R-INV))

;; word --> Boolean
;; Purpose: Determine if the given word does not contain abaa
(define (contains-no-abaa? w)

  (define FINALS '(S A B C))

  ;; state word --> Boolean
  ;; Purpose: Determine if the given word does not contain abaa
  (define (consume s w)
    (cond [(empty? w) (if (member s FINALS) #t #f)]
          [(and (eq? s 'S) (eq? 'a (first w)))
           (consume 'A (rest w))]
          [(and (eq? s 'S) (eq? 'b (first w)))
           (consume 'S (rest w))]
          [(and (eq? s 'A) (eq? 'a (first w)))
           (consume 'A (rest w))]
          [(and (eq? s 'A) (eq? 'b (first w)))
           (consume 'B (rest w))]
          [(and (eq? s 'B) (eq? 'a (first w)))
           (consume 'C (rest w))]
          [(and (eq? s 'B) (eq? 'b (first w)))
           (consume 'S (rest w))]
          [(and (eq? s 'C) (eq? 'a (first w)))
           (consume 'R (rest w))]
          [(and (eq? s 'C) (eq? 'b (first w)))
           (consume 'B (rest w))]
          [(and (eq? s 'R) (eq? 'a (first w)))
           (consume 'R (rest w))]
          [else (consume 'R (rest w))]))
  (consume 'S w))

;; Tests for contains?
(check-equal? (contains-no-abaa? '()) #t)
(check-equal? (contains-no-abaa? '(a b b a a)) #t)
(check-equal? (contains-no-abaa? '(b b b a b a b b a b a)) #t)
(check-equal? (contains-no-abaa? '(a b a a)) #f)
(check-equal? (contains-no-abaa? '(a b b b a b a a a)) #f)
(check-equal? (contains-no-abaa? '(a b a b a a a b a a b)) #f)

(define WORST-CASE (build-list 100000 (λ (i) 'a)))

(define BEST-CASE (append '(a b a a) (build-list 100000 (λ (i) 'a))))

(define T1 (time (contains? WORST-CASE PROHIBITED-PATTERN)))
(define T2 (time (contains-no-abaa? WORST-CASE)))

(define T3 (time (contains? BEST-CASE PROHIBITED-PATTERN)))
(define T4 (time (contains-no-abaa? BEST-CASE)))


