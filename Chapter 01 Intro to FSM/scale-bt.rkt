#lang racket

(require fsm rackunit)

;; A binary tree of numbers, (btof number), is either:
;;  1. '()
;;  2. number
;;  3. (list number (btof number) (btof number))

;; Function Template
;;; (btof number) ... --> ...
;;; Purpose: ...
;(define (f-on-bt a-bt ...)
;  (cond [(empty? a-bt) ...]
;        [(number? a-bt) ...(f-on-number a-bt)...]
;        [else ...(f-on-number (first a-bt))...(f-on-bt (second a-bt))...(f-on-bt (third a-bt))]))
;
;;; Tests
;(check-equals? (f-on-bt empty ...) ...)
;(check-equals? (f-on-bt number ...) ...)
;(check-equals? (f-on-bt (list number ... ...) ...) ...)
;     .
;     .
;     .

;; (btof number) number --> (btof number)
;; Purpose: Scale the given (btof number) by the given scalar
(define (scale-bt a-bt k)
  (cond [(empty? a-bt) a-bt]
        [(number? a-bt) (* k a-bt)]
        [else (list (* k (first a-bt))
                    (scale-bt (second a-bt) k)
                    (scale-bt (third a-bt) k))]))

;; Tests
(check-equal? (scale-bt '() 10) '())
(check-equal? (scale-bt -50 2) -100)
(check-equal? (scale-bt 40  8) 320)
(check-equal? (scale-bt (list 10 '() (list -8 -4 '())) -2)
              (list -20 '() (list 16 8 '())))
(check-equal? (scale-bt (list 0
                              (list 1 2 3)
                              (list 4
                                    (list 5 '() '())
                                    (list 6 7 8)))
                        3)
              (list 0
                    (list 3 6 9)
                    (list 12
                          (list 15 '() '())
                          (list 18 21 24))))

