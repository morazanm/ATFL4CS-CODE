#lang fsm

;; L(M) = ab*
(define M (make-dfa `(S F ,DEAD)
                    '(a b)
                    'S
                    '(F)
                    `((S a F)
                      (S b ,DEAD)
                      (F a ,DEAD)
                      (F b F)
                      (,DEAD a ,DEAD)
                      (,DEAD b ,DEAD))
                    'no-dead))

;; dfa --> tm
;; Purpose: Build a tm for the language of the given dfa
(define (dfa2tm m)
  (let [(accept-state (generate-symbol 'Y (sm-states m)))]
  (make-tm (cons accept-state (sm-states m))
           (sm-sigma m)
           (append
            (map (λ (f) (list (list f BLANK)
                              (list 'Y BLANK))) ;; only move to Y on a blank
                 (sm-finals m))
            (map (λ (r) (list (list (first r) (second r))
                              (list (third r) RIGHT)))
                 (sm-rules m)))
           (sm-start m)
           (list accept-state)
           accept-state)))

(define M-tm (dfa2tm M))

(check-equal? (sm-apply M '())
              (sm-apply M-tm `(,LM )))
(check-equal? (sm-apply M '(b b))
              (sm-apply M-tm `(,LM b b)))
(check-equal? (sm-apply M '(a b b a a))
              (sm-apply M-tm `(,LM a b b a a)))
(check-equal? (sm-apply M '(a))
              (sm-apply M-tm `(,LM a)))
(check-equal? (sm-apply M '(a b))
              (sm-apply M-tm `(,LM a b)))
(check-equal? (sm-apply M '(a b b))
              (sm-apply M-tm `(,LM a b b)))
(check-equal? (sm-apply M '(a b b b b b))
              (sm-apply M-tm `(,LM a b b b b b)))
