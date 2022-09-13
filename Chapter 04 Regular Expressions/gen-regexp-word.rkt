#lang racket

(require fsm rackunit)

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

(define MAX-KLEENESTAR-REPS 20)
              
;; regexp --> word
;; Purpose: Generate a random word in the language of the
;;          given regexp
;; Assume: The number of repetitions generated from a
;;         Kleene star regular expression does not 
;;         exceed MAX-KLEENESTAR-REPS
(define (gen-regexp-word rexp)

  ;; union-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps in the chain for the given union-regexp
  (define (extract-union-regexps urexp)
    (let [(r1 (union-regexp-r1 urexp))
          (r2 (union-regexp-r2 urexp))]
      (if (not (union-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-union-regexps r2)))))

  ;; concat-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps in the chain for the given concat-regexp
  (define (extract-concat-regexps crexp)
    (let [(r1 (concat-regexp-r1 crexp))
          (r2 (concat-regexp-r2 crexp))]
      (if (not (concat-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-concat-regexps r2)))))
  
  (cond [(empty-regexp? rexp) EMP]
        [(singleton-regexp? rexp)
         (let [(element (singleton-regexp-a rexp))]
           (if (not (string<=? "0" element "9"))
               (list (string->symbol element))
               (list (string->number element))))]
        [(kleenestar-regexp? rexp)
         (let* [(reps (random (add1 MAX-KLEENESTAR-REPS)))
                (element-list (flatten
                               (build-list
                                reps
                                (λ (i) (gen-regexp-word (kleenestar-regexp-r1 rexp))))))]
           (if (empty? element-list) EMP element-list))]
        [(union-regexp? rexp)
         (let* [(uregexps (extract-union-regexps rexp))
                (chosen (list-ref uregexps (random (length uregexps))))]
           (gen-regexp-word chosen))]
        [else (let [(cregexps (extract-concat-regexps rexp))]
                (filter (λ (w) (not (eq? w EMP)))
                        (flatten (map gen-regexp-word cregexps))))]))

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

(check-pred is-bin-nums? (gen-regexp-word BIN-NUMS))
(check-pred is-bin-nums? (gen-regexp-word BIN-NUMS))
(check-pred is-bin-nums? (gen-regexp-word BIN-NUMS))
(check-pred is-bin-nums? (gen-regexp-word BIN-NUMS))
(check-pred is-bin-nums? (gen-regexp-word BIN-NUMS))

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

(check-pred is-ends-with-a? (gen-regexp-word ENDS-WITH-A))
(check-pred is-ends-with-a? (gen-regexp-word ENDS-WITH-A))
(check-pred is-ends-with-a? (gen-regexp-word ENDS-WITH-A))
(check-pred is-ends-with-a? (gen-regexp-word ENDS-WITH-A))