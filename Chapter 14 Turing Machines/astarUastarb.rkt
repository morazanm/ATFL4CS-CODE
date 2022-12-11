#lang fsm

;; States
;;   S: no tape elements read, starting sate
;;   A: tape[1..i-1] has only a
;;   B: tape[1..i-1] has only a
;;   C: tape[1..i-2] has only a and tape[i-1] = b
;;   Y: tape[i] = BLANK and tape[1..i-1] = a* or a*b, final accepting state
;;   N: tape[1..i-1] != a* or a*b, final state

;; L = a* U a*b
;; PRE: tape = LMw AND i = 1
(define a*Ua*b (make-tm '(S A B C Y N)
                        `(a b)
                        `(((S a) (A ,RIGHT))
                          ((S a) (B ,RIGHT))
                          ((S ,BLANK) (Y ,RIGHT))
                          ((A a) (A ,RIGHT))
                          ((A ,BLANK) (Y ,BLANK))
                          ((B a) (B ,RIGHT))
                          ((B b) (C ,RIGHT))
                          ((C a) (N ,RIGHT))
                          ((C b) (N ,RIGHT))
                          ((C ,BLANK) (Y ,BLANK)))
                        'S
                        '(Y N)
                        'Y))

;; Tests for a*Ua*b
(check-equal? (sm-apply a*Ua*b `(,LM b b) 1) 'reject)
(check-equal? (sm-apply a*Ua*b `(,LM a a b a) 1) 'reject)
(check-equal? (sm-apply a*Ua*b `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a b) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a a a) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a a a b) 1) 'accept)

;; tape natum --> Boolean
;; Purpose: Determine that no tape elements read
(define (S-INV t i) (= i 1))

(check-equal? (S-INV `(,LM a a) 2) #f)
(check-equal? (S-INV `(,LM a a b) 1) #t)


;; tape natum --> Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (A-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

(check-equal? (A-INV `(,LM ,BLANK ,BLANK) 2) #f)
(check-equal? (A-INV `(,LM a) 0) #f)
(check-equal? (A-INV `(,LM a b a a a) 3) #f)
(check-equal? (A-INV `(,LM a ,BLANK) 2) #t)
(check-equal? (A-INV `(,LM a a a b) 4) #t)

;; tape natum --> Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (B-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

(check-equal? (B-INV `(,LM ,BLANK ,BLANK) 2) #f)
(check-equal? (B-INV `(,LM a) 0) #f)
(check-equal? (B-INV `(,LM a b a a a) 5) #f)
(check-equal? (B-INV `(,LM a ,BLANK) 2) #t)
(check-equal? (B-INV `(,LM a a a b) 3) #t)


;; tape natum --> Boolean
;; Purpose: Determine that tape[1..i-2] has only a and tape[i-1] = b
(define (C-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (- i 2)))
       (eq? (list-ref t (sub1 i)) 'b)))

(check-equal? (C-INV `(,LM ,BLANK ,BLANK) 2) #f)
(check-equal? (C-INV `(,LM a a b b a) 5) #f)
(check-equal? (C-INV `(,LM a a b b a) 4) #t)
(check-equal? (C-INV `(,LM b ,BLANK) 2) #t)


;; tape natum --> Boolean
;; Purpose: Determine that tape[i] = BLANK and
;;          tape[1..i-1] = a* or tape[1..i-1] = a*b
(define (Y-INV t i)
  (or (and (= i 2) (eq? (list-ref t (sub1 i)) BLANK))
      (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))
      (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
             (back (takef (drop t (add1 (length front)))
                          (λ (s) (not (eq? s BLANK)))))]
        (equal? back '(b)))))

(check-equal? (Y-INV `(,LM a a b a) 4) #f)
(check-equal? (Y-INV `(,LM b a a) 3) #f)
(check-equal? (Y-INV `(,LM ,BLANK ,BLANK) 2) #t)
(check-equal? (Y-INV `(,LM a a a a ,BLANK) 5) #t)
(check-equal? (Y-INV `(,LM a a a a a b ,BLANK) 7) #t)


;; tape natum --> Boolean
;; Purpose: Determine that tape[1..i-1] != a* or a*b
(define (N-INV t i)
  (and (not (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i))))
       (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
              (back (takef (drop t (add1 (length front)))
                           (λ (s) (not (eq? s BLANK)))))]
         (not (equal? back '(b))))))

(check-equal? (N-INV `(,LM ,BLANK) 1) #f)
(check-equal? (N-INV `(,LM b ,BLANK) 1) #f)
(check-equal? (N-INV `(,LM a b ,BLANK) 3) #f)
(check-equal? (N-INV `(,LM a a a ,BLANK) 4) #f)
(check-equal? (N-INV `(,LM a b a ,BLANK) 4) #t)
(check-equal? (N-INV `(,LM a a a b b ,BLANK) 5) #t)

#;(sm-visualize a*Ua*b
              (list 'S S-INV) (list 'A A-INV)
              (list 'B B-INV) (list 'C C-INV)
              (list 'Y Y-INV) (list 'N N-INV))