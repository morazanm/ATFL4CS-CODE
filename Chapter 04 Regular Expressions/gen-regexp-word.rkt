#lang fsm

;; Sample regexps
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

;; Template for functions on a regular expression
;;; regexp ... \arrow ...
;;; Purpose: ...
;(define (f-on-regexp rexp ...)
;  (cond [(empty-regexp? rexp) ...]
;        [(singleton-regexp? rexp) 
;         ...(singleton-regexp-a rexp)...]
;        [(kleenestar-regexp? rexp) 
;         ...(f-on-regexp (kleenestar-regexp-r1 rexp))...]
;        [(union-regexp? rexp) 
;         ...(f-on-regexp (union-regexp-r1 rexp))...
;         ...(f-on-regexp (union-regexp-r2 rexp))...]
;        [else ...(f-on-regexp (concat-regexp-r1 rexp))...
;              ...(f-on-regexp (concat-regexp-r2 rexp))...]))


              
;; regexp [natnum] --> word
;; Purpose: Generate a random word in the language of the
;;          given regexp such that the number of repetitions
;;          generated from a Kleene star regular expression  
;;          does not exceed (max 20 reps).
(define (gen-word rexp . reps)
  (define MAX-KLEENESTAR-REPS (if (null? reps) 20 (first reps)))
  
  ;; regexp --> word
  ;; Purpose: Generate a word in the language of the given regexp
  (define (generate rexp)
    (cond [(empty-regexp? rexp) EMP]
          [(singleton-regexp? rexp) (convert-singleton rexp)]
          [(kleenestar-regexp? rexp)
           (let* [(reps (pick-reps MAX-KLEENESTAR-REPS))
                  (low (flatten
                        (filter
                         (λ (w) (not (eq? w EMP)))
                         (build-list
                          reps
                          (lambda (i)
                            (generate (kleenestar-regexp-r1 rexp)))))))]
             (if (empty? low) EMP low))]
          [(union-regexp? rexp) (generate (pick-regexp rexp))]
          [else
           (let [(w1 (gen-word (concat-regexp-r1 rexp)))
                 (w2 (gen-word (concat-regexp-r2 rexp)))]
             (cond [(and (eq? w1 EMP) (eq? w2 EMP)) EMP]
                   [(eq? w1 EMP) w2]
                   [(eq? w2 EMP) w1]
                   [else (append w1 w2)]))]))
  (generate rexp))

;; Tests

;; Tests
;; word --> Boolean
;; Purpose: Test if the given word is in BIN-NUMS
(define (is-bin-nums? w)
  (and (list? w)
       (>= (length w) 1)
       (or (equal? w '(0)) (= (first w) 1))
       (andmap (λ (bit) (or (= bit 0) (= bit 1))) w)))

(check-equal? (is-bin-nums? '(0)) #t)
(check-equal? (is-bin-nums? '(1 0 0 1 0 1 1)) #t)
(check-equal? (is-bin-nums? '(1 1 1 0 1 0 0 0 1 1 0 1)) #t)
(check-equal? (is-bin-nums? '()) #f)
(check-equal? (is-bin-nums? '(0 0 0 1 1 0 1 0)) #f)

(check-pred is-bin-nums? (gen-word BIN-NUMS))
(check-pred is-bin-nums? (gen-word BIN-NUMS))
(check-pred is-bin-nums? (gen-word BIN-NUMS))
(check-pred is-bin-nums? (gen-word BIN-NUMS 30))
(check-pred is-bin-nums? (gen-word BIN-NUMS 50))

;; word --> Boolean
;; Purpose: Test if the given word is in ENDS-WITH-A
(define (is-ends-with-a? w)
  (and (list? w)
       (>= (length w) 1)
       (eq? (last w) 'a)))

(check-equal? (is-ends-with-a? '(a)) #t)
(check-equal? (is-ends-with-a? '(b b a)) #t)
(check-equal? (is-ends-with-a? '(a b b a b a)) #t)
(check-equal? (is-ends-with-a? '()) #f)
(check-equal? (is-ends-with-a? '(b b b)) #f)
(check-equal? (is-ends-with-a? '(a a a a b)) #f)

(check-pred is-ends-with-a? (gen-word ENDS-WITH-A))
(check-pred is-ends-with-a? (gen-word ENDS-WITH-A))
(check-pred is-ends-with-a? (gen-word ENDS-WITH-A 18))
(check-pred is-ends-with-a? (gen-word ENDS-WITH-A 7))
