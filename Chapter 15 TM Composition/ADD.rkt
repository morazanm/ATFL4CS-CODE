#lang fsm

;; State documentation
;; S: i = 0 and tape = (LM BLANK a BLANK b)
;; A: i <= numd(a) + 2 and tape = (LM BLANK a BLANK b)
;; B: i <= numd(a) + numd(b) + 3 and tape = (LM BLANK a d b)
;; C: i = numd(a) + numd(b) + 2 and tape = (LM BLANK a b) 
;; D: i = numd(a) + numd(b) + 2 and tape = (LM BLANK a b)
;; E: i = numd(a) + numd(b) + 1 and tape = (LM BLANK a b)
;; F: i = numd(a) + numd(b) + 3 if a = b = 0
;;      = numd(a) + numd(b) + 2 otherwise
;;    and tape = (LM BLANK a b)
;; G: i = numd(a) + numd(b) + 2 and tape = (LM BLANK)

;;  PRE: tape = (LM BLANK a BLANK b) AND i = 1
;; POST: tape = (LM BLANK a b) AND
;;       i = numd(a) + numd(b) + 3 if a = b = 0
;;         = numd(a) + numd(b) + 2 otherwise
(define ADD (make-tm '(S A B C D E F G)
                     '(d)
                     `(((S ,BLANK) (A ,RIGHT))
                       ((A d) (A ,RIGHT))
                       ((A ,BLANK) (B d))
                       ((B d) (B ,RIGHT))
                       ((B ,BLANK) (C ,LEFT))
                       ((C d) (D ,BLANK))
                       ((D ,BLANK) (E ,LEFT))
                       ((E d) (F ,RIGHT))
                       ((E ,BLANK) (G ,RIGHT))
                       ((G ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK ,BLANK ,BLANK) 1))
              `(F 3 (,LM ,BLANK ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK ,BLANK d d d) 1))
              `(F 5 (,LM ,BLANK d d d ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK d d) 1))
              `(F 4 (,LM ,BLANK d d ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK d d ,BLANK d d d) 1))
              `(F 7 (,LM ,BLANK d d d d d ,BLANK ,BLANK)))