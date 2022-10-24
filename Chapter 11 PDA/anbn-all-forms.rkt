#lang racket

(require fsm rackunit)

;; word --> Boolean
;; Purpose: Decide if given word is in a^nb^n
(define (is-in-a^nb^n? w)
  ;; word (listof symbol) --> Boolean
  ;; Purpose: Determine
  ;; Accumulator Invariant
  ;;   acc = the unmatched as at the beginning of w
  ;; Assume: w in (a b)*
  (define (check wrd acc)
    (cond [(empty? wrd) (empty? acc)]
          [(eq? (first wrd) 'a) #f]
          [else (and (not (empty? acc))
                     (check (rest wrd) (rest acc)))]))
  (check (dropf w (λ (s) (eq? s 'a)))
         (takef w (λ (s) (eq? s 'a)))))

;; Tests for is-in-anbn?
(check-pred (λ (w) (not (is-in-a^nb^n? w))) '(a))
(check-pred (λ (w) (not (is-in-a^nb^n? w))) '(b b))
(check-pred (λ (w) (not (is-in-a^nb^n? w))) '(a b b))
(check-pred (λ (w) (not (is-in-a^nb^n? w))) '(a b a a b b))
(check-pred is-in-a^nb^n? '())
(check-pred is-in-a^nb^n? '(a a b b))

;;; word --> Boolean
;;; Purpose: Decide if given word is in a^nb^n
;(define (is-in-a^nb^n2? w)
;  (and (even? (length w))
;       (let [(as (take w (/ (length w) 2)))
;             (bs (drop w (/ (length w) 2)))]
;         (= (length as) (length bs)))))
;
;;; Tests for is-in-anbn?
;(check-pred (λ (w) (not (is-in-a^nb^n2? w))) '(a))
;(check-pred (λ (w) (not (is-in-a^nb^n2? w))) '(a b b))
;(check-pred is-in-a^nb^n2? '())
;(check-pred is-in-a^nb^n2? '(a a b b))

;; L = {a^nb^n | n >= 0}
;; States
;;  S ci = (listof a) = stack, start state
;;  M ci = (append (listof a) (listof b)) AND
;;    stack only contains a's
;;    (length ci as) = (length stack) + (length ci bs) 
;;  F ci = (append (listof a) (listof b)) and all as and bs matched, final state
;; The stack is a (listof a)
(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

;; Tests for a^nb^n
(check-equal? (sm-apply a^nb^n '(a)) 'reject)
(check-equal? (sm-apply a^nb^n '(b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b a a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '()) 'accept)
(check-equal? (sm-apply a^nb^n '(a a b b)) 'accept)

;; word stack --> Boolean
;; Purpose: Determine if the given ci and stack are the same (listof a)
(define (S-INV ci stck)
  (and (= (length ci) (length stck))
       (andmap (λ (i g) (and (eq? i 'a) (eq? g 'a))) ci stck)))

;; Tests for S-INV
(check-equal? (S-INV '()'(a a)) #f)
(check-equal? (S-INV '(a)'()) #f)
(check-equal? (S-INV '(b b b)'(b b b)) #f)
(check-equal? (S-INV '()'()) #t)
(check-equal? (S-INV '(a a a)'(a a a)) #t)

;; word stack --> Boolean
;; Purpose: Determine if ci = EMP or a+b+ AND ci as = stack + ci bs
(define (M-INV ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as)) (λ (s) (eq? s 'b))))]
    (and (equal? (append as bs) ci)
         (andmap (λ (s) (eq? s 'a)) stck)
         (= (length as) (+ (length bs) (length stck))))))

;; Tests for M-INV
(check-equal? (M-INV '(a a b)'(a a)) #f)
(check-equal? (M-INV '(a)'()) #f)
(check-equal? (M-INV '(a a a b)'(a a a)) #f)
(check-equal? (M-INV '(a a a b)'(a)) #f)
(check-equal? (M-INV '()'()) #t)
(check-equal? (M-INV '(a)'(a)) #t)
(check-equal? (M-INV '(a b)'()) #t)
(check-equal? (M-INV '(a a a b b)'(a)) #t)

;; word stack --> Boolean
;; Purpose: Determine if ci = a^nb^n and stack is empty
(define (F-INV ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as)) (λ (s) (eq? s 'b))))]
    (and (empty? stck)
         (equal? (append as bs) ci)
         (= (length as) (length bs)))))

;; Tests for F-INV
(check-equal? (F-INV '(a a b)'()) #f)
(check-equal? (F-INV '(a)'()) #f)
(check-equal? (F-INV '(a a a b)'(a a a)) #f)
(check-equal? (F-INV '()'()) #t)
(check-equal? (F-INV '(a b)'()) #t)
(check-equal? (F-INV '(a a b b)'()) #t)

;(sm-visualize a^nb^n (list 'S S-INV) (list 'M M-INV) (list 'F F-INV))