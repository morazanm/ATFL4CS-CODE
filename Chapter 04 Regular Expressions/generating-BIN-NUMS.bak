#lang racket

(require fsm rackunit)

;;  --> BIN-NUMS
;; Purpose: Generate a binary number without leading zeroes of length <= MAX-LENGTH
(define (generate-bn)
  
  (define MAX-LENGTH 50)

  ;; natnum --> BIN-NUMS
  ;; Purpose: Generate a random word of bits of the given length
  (define (generate-0U1* n)

    ;;  --> bit
    ;; Purpose: Generate a random bit
    (define (generate-bit) (if (< (random) 0.5) 0 1))
    
    (if (= n 0)
        '()
        (cons (generate-bit) (generate-0U1* (sub1 n)))))
        
  (if (< (random) 0.01)
      (list 0)
      (cons 1 (generate-0U1* (random MAX-LENGTH)))))

;; Tests
;; word --> Boolean
;; Purpose: Test if the given word is a BIN-NUMS
(define (is-bin-nums? w)
  (and (list? w)
       (<= 1 (length w))
       (or (equal? w '(0)) (= (first w) 1))
       (andmap (λ (bit) (or (= bit 0) (= bit 1))) w)))

(check-equal? (is-bin-nums? '(0)) #t)
(check-equal? (is-bin-nums? '(1 0 0 1 0 1 1)) #t)
(check-equal? (is-bin-nums? '()) #f)
(check-equal? (is-bin-nums? '(0 0 0 1 1 0 1 0)) #f)
(check-equal? (is-bin-nums? '(1 1 1 0 1 0 0 0 1 1 0 1)) #t)

(check-pred is-bin-nums? (generate-bn))
(check-pred is-bin-nums? (generate-bn))
(check-pred is-bin-nums? (generate-bn))
(check-pred is-bin-nums? (generate-bn))
(check-pred is-bin-nums? (generate-bn))