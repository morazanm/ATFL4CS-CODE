#lang fsm

;; States (i = head's position)
;;   S: tape[1..i-1] only contains as, starting state
;;   Y: tape[i] = BLANK and tape[1..i-1] only contains as, final state
;;   N: tape[i] = b and tape[1..i-1] contains only as, final state

;; L = a*
;; PRE: tape = LMw_ AND i = 0
(define a* (make-tm '(S Y N)
                    `(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y))

;; Tests for a*
(check-equal? (sm-apply a* `(,LM a a a b a a)) 'reject)
(check-equal? (sm-apply a* `(,LM b a a)) 'reject)
(check-equal? (sm-apply a* `(,LM)) 'accept)
(check-equal? (sm-apply a* `(,LM a a a)) 'accept)

;; tape natnum --> Boolean
;; Purpose: Everything in tape[1..i-1] is an a
;; skip LM & ith not read
(define (S-INV t i)
  (or (= i 0)
      (andmap 
       (λ (s) (eq? s 'a))
       (take (rest t) (sub1 i)))))

(check-equal? (S-INV `(,LM b ,BLANK) 2) #f)
(check-equal? (S-INV `(,LM a a b a a) 4) #f)
(check-equal? (S-INV `(,LM) 0) #t)
(check-equal? (S-INV `(,LM b) 1) #t)
(check-equal? (S-INV `(,LM a ,BLANK) 2) #t)
(check-equal? (S-INV `(,LM a a a a ,BLANK) 5) #t)

;; tape natnum --> Boolean
;; Purpose: Everything in tape[1..i-1] is and tape[i] = BLANK
;; skip LM
(define (Y-INV t i)
  (and (eq? (list-ref t i) BLANK)
       (andmap (λ (s) (eq? s 'a)) (take (cdr t) (sub1 i)))))

;; Tests for Y-INV
(check-equal? (Y-INV `(,LM b ,BLANK) 2) #f)
(check-equal? (Y-INV `(,LM a b a ,BLANK) 2) #f)
(check-equal? (Y-INV `(,LM a b a ,BLANK) 4) #f)
(check-equal? (Y-INV `(,LM ,BLANK) 1) #t)
(check-equal? (Y-INV `(,LM a a a ,BLANK) 4) #t)

;; tape natnum --> Boolean
;; Purpose: Determine that tape[i] = b
;; skip LM
(define (N-INV t i)
  (eq? (list-ref t i) 'b)
  #;(ormap (λ (s) (eq? s 'b)) (take (cdr t) (sub1 i))))

;; Tests for N-INV
(check-equal? (N-INV `(,LM ,BLANK) 1) #f)
(check-equal? (N-INV `(,LM a a ,BLANK) 3) #f)
(check-equal? (N-INV `(,LM a b a b ,BLANK) 4) #t)
(check-equal? (N-INV `(,LM b b b) 2) #t)
(check-equal? (N-INV `(,LM a a a a b ,BLANK) 5) #t)
