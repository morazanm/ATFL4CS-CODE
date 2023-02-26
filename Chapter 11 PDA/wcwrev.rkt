#lang fsm

;; L = {wcw^R | w in (a b)*}
;; States
;;  S ci is empty and stack is empty
;;  P ci = stack^R AND c not in ci
;;  Q ci = (append w (list 'c) v) AND
;;     w = stack^R v^R
;;  F stack = '() AND ci = (append w (list c) w^R)
(define wcw^r (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))

;; Tests for wcw^r
(check-equal? (sm-apply wcw^r '(a)) 'reject)
(check-equal? (sm-apply wcw^r '(a c)) 'reject)
(check-equal? (sm-apply wcw^r '(b c a)) 'reject)
(check-equal? (sm-apply wcw^r '(a a b c b a b)) 'reject)
(check-equal? (sm-apply wcw^r '(c)) 'accept)
(check-equal? (sm-apply wcw^r '(a c a)) 'accept)
(check-equal? (sm-apply wcw^r '(a b b b c b b b a)) 'accept)

;; word stack --> Boolean
;; Purpose: Determine in the given word and stack are empty
(define (S-INV ci s) (and (empty? ci) (empty? s)))

;; Tests for S-INV
(check-equal? (S-INV '() '(a a)) #f)
(check-equal? (S-INV '(a c a) '()) #f)
(check-equal? (S-INV '(a c a) '(b b)) #f)
(check-equal? (S-INV '() '()) #t)

;; word stack --> Boolean
;; Purpose: Determine if the given ci is the reverse of the given stack AND c is not in ci
(define (P-INV ci s)
  (and (equal? ci (reverse s)) (not (member 'c ci))))

;; Tests for P-INV
(check-equal? (P-INV '(a c a) '(a c a)) #f)
(check-equal? (P-INV '(a a) '(a b)) #f)
(check-equal? (P-INV '() '()) #t)
(check-equal? (P-INV '(a b) '(b a)) #t)
(check-equal? (P-INV '(a b a a) '(a a b a)) #t)

;; word stack --> Boolean
;; Purpose: Determine if ci=s^Rv^Rcv AND and w=s^Rv^R
(define (Q-INV ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))
         (v (if (member 'c ci) (drop ci (add1 (length w))) '()))]
    (and (equal? ci (append w (list 'c) v))
         (equal? w (append (reverse s) (reverse v))))))

;; Tests for Q-INV
(check-equal? (Q-INV '(a a) '()) #f)
(check-equal? (Q-INV '(b b c a) '(b a)) #f)
(check-equal? (Q-INV '(c) '()) #t)
(check-equal? (Q-INV '(b a c) '(a b)) #t)
(check-equal? (Q-INV '(a b c b) '(a)) #t)
(check-equal? (Q-INV '(a b b c b) '(b a)) #t)

;; word stack --> Boolean
;; Purpose: Determine if ci=s^Rv^Rcv AND stack is empty
(define (F-INV ci s)
  (let [(w (takef ci (λ (s) (not (eq? s 'c)))))]
    (and (empty? s)
         (equal? ci (append w (list 'c) (reverse w))))))

;; Tests for F-INV
(check-equal? (F-INV '() '()) #f)
(check-equal? (F-INV '(b b) '()) #f)
(check-equal? (F-INV '(b a c) '(b a)) #f)
(check-equal? (F-INV '(c) '()) #t)
(check-equal? (F-INV '(b a c a b) '()) #t)
(check-equal? (F-INV '(a b b c b b a) '()) #t)

;(sm-visualize wcw^r (list 'S S-INV) (list 'P P-INV) (list 'Q Q-INV) (list 'F F-INV))
