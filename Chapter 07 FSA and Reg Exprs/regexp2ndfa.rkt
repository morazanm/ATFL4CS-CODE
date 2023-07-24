#lang fsm

(require "closure-algorithms.rkt")

;; regexp alphabet --> ndfa
;; Purpose: Build an ndfa for the given regexp
(define (regexp->ndfa e sigma)
  (let* [(simple-tbl (map (λ (a)
                            (let [(S (generate-symbol 'S '(S)))
                                  (A (generate-symbol 'A '(A)))]
                              (list a (make-ndfa (list S A)
                                                 sigma
                                                 S
                                                 (list A)
                                                 (list (list S a A))))))
                          (cons EMP sigma)))]
    (cond [(empty-regexp? e)
           (second (assoc EMP simple-tbl))]
          [(singleton-regexp? e)
           (second (assoc (string->symbol (singleton-regexp-a e)) simple-tbl))]
          [(concat-regexp? e)
           (concat-fsa (regexp->ndfa (concat-regexp-r1 e) sigma)
                       (regexp->ndfa (concat-regexp-r2 e) sigma))]
          [(union-regexp? e)
           (union-fsa (regexp->ndfa (union-regexp-r1 e) sigma)
                      (regexp->ndfa (union-regexp-r2 e) sigma))]
          [else (kstar-fsa (regexp->ndfa (kleenestar-regexp-r1 e) sigma))])))

;; Tests for reg-exp->ndfa
(define e (empty-regexp))
(define a (singleton-regexp "a"))
(define b (singleton-regexp "b"))
(define ab (concat-regexp a b))
(define aa (concat-regexp a a))
(define abUe (union-regexp ab e))
(define abUaa (union-regexp ab aa))
(define aa-* (kleenestar-regexp aa))
(define abUaa-* (kleenestar-regexp abUaa))

(define Me (regexp->ndfa e '(a b)))
(define Ma (regexp->ndfa a '(a b)))
(define Mb (regexp->ndfa b '(a b)))
(define Mab (regexp->ndfa ab '(a b)))
(define Maa (regexp->ndfa aa '(a b)))
(define MabUMe (regexp->ndfa abUe '(a b)))
(define MabUaa (regexp->ndfa abUaa '(a b)))
(define Maa-* (regexp->ndfa aa-* '(a b)))
(define MabUaa-* (regexp->ndfa abUaa-* '(a b)))

(check-equal? (sm-apply Me '(a)) 'reject)
(check-equal? (sm-apply Me '()) 'accept)
(check-equal? (sm-apply Ma '(b)) 'reject)
(check-equal? (sm-apply Ma '(a)) 'accept)
(check-equal? (sm-apply Mab '()) 'reject)
(check-equal? (sm-apply Mab '(a b)) 'accept)
(check-equal? (sm-apply Maa '(b a a)) 'reject)
(check-equal? (sm-apply Maa '(a a)) 'accept)
(check-equal? (sm-apply MabUMe '(a b a a)) 'reject)
(check-equal? (sm-apply MabUMe '(b b)) 'reject)
(check-equal? (sm-apply MabUMe '()) 'accept)
(check-equal? (sm-apply MabUMe '(a b)) 'accept)
(check-equal? (sm-apply MabUaa '(a b b b)) 'reject)
(check-equal? (sm-apply MabUaa '(b a b)) 'reject)
(check-equal? (sm-apply MabUaa '(a a)) 'accept)
(check-equal? (sm-apply MabUaa '(a b)) 'accept)
(check-equal? (sm-apply Maa-* '(a b)) 'reject)
(check-equal? (sm-apply Maa-* '(a a a)) 'reject)
(check-equal? (sm-apply Maa-* '(a a)) 'accept)
(check-equal? (sm-apply Maa-* '(a a a a a a)) 'accept)
(check-equal? (sm-apply MabUaa-* '(a b a)) 'reject)
(check-equal? (sm-apply MabUaa-* '(b b b b)) 'reject)
(check-equal? (sm-apply MabUaa-* '()) 'accept)
(check-equal? (sm-apply MabUaa-* '(a a a a a b)) 'accept)



