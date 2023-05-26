#lang fsm

;; Determinitic tm-language-recognizer --> Deterministic tm-language-recognizer
;; Purpose: Build a deterministic tm-language-recognizer for the complement
;;          of the language decided by the given deterministic tm-language-recognizer
;; Assumption: Given tm's final states are Y and N and Y is the accepting state
(define (dtm4L->dtm4notL M)
  (make-tm (sm-states M)
           (sm-sigma M)
           (sm-rules M)
           (sm-start M)
           (sm-finals M)
           'N))

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


(define Not-a* (dtm4L->dtm4notL a*))

(check-equal? (sm-apply Not-a* `(,LM a a a b a a)) 'accept)
(check-equal? (sm-apply Not-a* `(,LM b a a)) 'accept)
(check-equal? (sm-apply Not-a* `(,LM)) 'reject)
(check-equal? (sm-apply Not-a* `(,LM a a a)) 'reject)
           