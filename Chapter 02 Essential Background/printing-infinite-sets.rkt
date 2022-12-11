#lang fsm

;;  --> (void)
;; Purpose: Print the natural numbers
(define (print-natnums)
  ;; natnum --> (void)
  ;; Purpose: Print the natural numbers starting with the given natural number
  (define (printer n)
    (if (= n +inf.0)
        (void)
        (begin
          (displayln n)
          (printer (add1 n)))))
  (printer 0))

;;  --> (void)
;; Purpose: Print the even natural numbers
(define (print-evennatnums)
  (define (printer n)
    ;; natnum --> (void)
    ;; Purpose: Print the multiples of 2 starting with the
    ;;          multiple of 2 for the given natural number
    (if (= n +inf.0)
        (void)
        (begin
          (displayln (* 2 n))
          (printer (add1 n)))))
  (printer 0))

;;  --> (void)
;; Purpose: Print the multiples of 3
(define (print-mults3)
  ;; natnum --> (void)
  ;; Purpose: Print the multiples of 3 starting with the
  ;;          multiple of 3 for the given natural number
  (define (printer n)
    (if (= n +inf.0)
        (void)
        (begin
          (displayln (* 3 n))
          (printer (add1 n)))))
  (printer 0))

;;  --> (void)
;; Purpose: Print the elements of a*
(define (print-a*)
  ;; natnum --> (void)
  ;; Purpose: Print words of a's starting with the word
  ;;          with the given number of a's
  (define (printer n)
    (if (= n +inf.0)
        (void)
        (begin
          (displayln (list->string (build-list n (λ (i) #\a))))
          (printer (add1 n)))))
  (printer 0))

;; BIGSET = EVENNATS U MULTS3 U A*

;;  --> (void)
;; Purpose: Print the elements of BIGSET
(define (print-bigset)
  ;; natnum --> (void)
  ;; Purpose: Print the elements of EVENNATS, MULTS3, and A* starting
  ;;          with the elements indexed by the given natural number
  (define (printer n)
    (if (= n +inf.0)
        (void)
        (begin
          (displayln (* 2 n))
          (displayln (* 3 n))
          (displayln (list->string (build-list n (λ (i) #\a))))
          (printer (add1 n)))))
  (printer 0))

