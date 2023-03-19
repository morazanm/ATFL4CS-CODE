#lang fsm

;; L = wcw^R
;; State Documentation
;;  S: ci = {a b}* AND stack = ci^R
;;  F: ci = xycy^R AND stack = x^R
(define dwcw^r (make-ndpda '(S F)
                           '(a b c)
                           '(a b)
                           'S
                           '(F)
                           `(((S a ,EMP) (S (a)))
                             ((S b ,EMP) (S (b)))
                             ((S c ,EMP) (F ,EMP))
                             ((F a (a)) (F ,EMP))
                             ((F b (b)) (F ,EMP)))))

(check-equal? (sm-apply dwcw^r '()) 'reject)
(check-equal? (sm-apply dwcw^r '(a b b a)) 'reject)
(check-equal? (sm-apply dwcw^r '(a a)) 'reject)
(check-equal? (sm-apply dwcw^r '(a b b c b b a b)) 'reject)
(check-equal? (sm-apply dwcw^r '(a c a)) 'accept)
(check-equal? (sm-apply dwcw^r '(b a c a b)) 'accept)
(check-equal? (sm-apply dwcw^r '(b b a c a b b)) 'accept)


;; word stack --> Boolean
(define (S-INV ci s)
  (and (andmap (λ (s) (or (eq? s 'a) (eq? s 'b)))
               ci)
       (equal? ci (reverse s))))

(check-equal? (S-INV '(c) '()) #f)
(check-equal? (S-INV '(b a) '(b a)) #f)
(check-equal? (S-INV '() '()) #t)
(check-equal? (S-INV '(a b b) '(b b a)) #t)

;; word stack --> Boolean
(define (F-INV ci s)
  (and (member 'c ci)
       (let* [(w (takef ci (λ (x) (not (eq? x 'c)))))
              (x (take ci (length s)))
              (y^R (drop ci (add1 (length w))))]
         (and (equal? ci (append x (reverse y^R) (list 'c) y^R))
              (equal? s (reverse x))))))

(check-equal? (F-INV '(a b c) '()) #f)
(check-equal? (F-INV '(a a c) '(a)) #f)
(check-equal? (F-INV '(a b) '(b a)) #f)
(check-equal? (F-INV '(c) '()) #t)
(check-equal? (F-INV '(b a c) '(a b)) #t)
(check-equal? (F-INV '(a b b c b) '(b a)) #t)


;(sm-visualize dwcw^r (list 'S S-INV) (list 'F F-INV))


















