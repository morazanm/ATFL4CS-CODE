#lang fsm

(require "ndfa2dfa.rkt")

(provide concat-fsa union-fsa kstar-fsa)

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))
                       'no-dead))

;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
                            '(a b)
                            'Z
                            '(F)
                            `((Z a H)
                              (Z a B)
                              (H a D)
                              (D ,EMP F)
                              (B a C)
                              (C b F)
                              (F b F))))

;; L = aab*
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))

;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))

;; L = {w | w has an even number of a and an odd number of b}
(define EVEN-A-ODD-B (make-dfa '(S M N P)
                               '(a b)
                               'S
                               '(N)
                               '((S a P)
                                 (S b N)
                                 (M a N)
                                 (M b P)
                                 (N a M)
                                 (N b S)
                                 (P a S)
                                 (P b M))
                               'no-dead))


;; ndfa ndfa --> ndfa
;; Purpose: Construct an ndfa for the union of the languages of the given ndfas
;; Assume: The intersection of the states of the given machines is empty
(define (union-fsa M N)
  (let* [(new-start (generate-symbol 'S (append (sm-states M) (sm-states N))))
         (new-sigma (remove-duplicates (append (sm-sigma M) (sm-sigma N))))
         (new-states (cons new-start (append (sm-states M) (sm-states N))))
         (new-finals (append (sm-finals M) (sm-finals N)))
         (new-rules (append (list (list new-start EMP (sm-start M))
                                  (list new-start EMP (sm-start N)))
                            (sm-rules M)
                            (sm-rules N)))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))

;; Tests for union-fsa
(define ab*Ua-aUb-b* (union-fsa ab* a-aUb-b*))
(define ab*Uaab* (union-fsa ab* aab*))

(check-equal? (sm-apply ab*Ua-aUb-b* '()) 'reject)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a a a a)) 'reject)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a b)) 'accept)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a a b b)) 'accept)
(check-equal? (sm-testequiv? ab*Ua-aUb-b* (sm-union ab* ab*Ua-aUb-b*)) #t)

(check-equal? (sm-apply ab*Uaab* '(a a a)) 'reject)
(check-equal? (sm-apply ab*Uaab* '(b a b a)) 'reject)
(check-equal? (sm-apply ab*Uaab* '(a b b)) 'accept)
(check-equal? (sm-apply ab*Uaab* '(a a b)) 'accept)
(check-equal? (sm-apply ab*Uaab* '(a b b b b)) 'accept)
(check-equal? (sm-testequiv? ab*Uaab* (sm-union ab* aab*)) #t)



;; ndfa ndfa --> ndfa
;; Purpose: Construct an ndfa for the concatenation of the languages of the given ndfas
;; Assume: The intersection of the states of the given machines is empty
(define (concat-fsa M N)
  (let* [(new-start (sm-start M))
         (new-sigma (remove-duplicates (append (sm-sigma M) (sm-sigma N))))
         (new-states (append (sm-states M) (sm-states N)))
         (new-finals (sm-finals N))
         (new-rules (append (sm-rules M)
                            (sm-rules N)
                            (map (λ (f) (list f EMP (sm-start N))) (sm-finals M))))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))

;; Tests for concat-fsa
(define ab*-o-a-aUb-b* (concat-fsa ab* a-aUb-b*))
(define ab*-o-aab* (concat-fsa ab* aab*))

(check-equal? (sm-apply ab*-o-a-aUb-b* '()) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(b b b)) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a a b a b)) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a b a a b)) 'accept)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a b b b a a)) 'accept)
(check-equal? (sm-testequiv? ab*-o-a-aUb-b* (sm-concat ab* a-aUb-b*)) #t)

(check-equal? (sm-apply ab*-o-aab* '()) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a b a)) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a a b b a a)) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a b b a a b b)) 'accept)
(check-equal? (sm-apply ab*-o-aab* '(a a a)) 'accept)
(check-equal? (sm-testequiv? ab*-o-aab* (sm-concat ab* aab*)) #t)

;; ndfa --> ndfa
;; Purpose: Construct an ndfa for the Kleene star of given ndfa's language
(define (kstar-fsa M)
  (let* [(new-start (generate-symbol 'K (sm-states M)))
         (new-sigma (sm-sigma M))
         (new-states (cons new-start (sm-states M)))
         (new-finals (cons new-start (sm-finals M)))
         (new-rules (cons (list new-start EMP (sm-start M))
                          (append (sm-rules M)
                                  (map (λ (f) (list f EMP new-start))
                                       (sm-finals M)))))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))

;; Tests for kstar-fsa
(define a-aUb-b*-* (kstar-fsa a-aUb-b*))
(define ab*-* (kstar-fsa ab*))

(check-equal? (sm-apply a-aUb-b*-* '(b b b)) 'reject)
(check-equal? (sm-apply a-aUb-b*-* '(a b a b a a a a)) 'reject)
(check-equal? (sm-apply a-aUb-b*-* '()) 'accept)
(check-equal? (sm-apply a-aUb-b*-* '(a a a a b b b b)) 'accept)
(check-equal? (sm-apply a-aUb-b*-* '(a a b a a b b a a)) 'accept)
(check-equal? (sm-testequiv? a-aUb-b*-* (sm-kleenestar a-aUb-b*)) #t)

(check-equal? (sm-apply ab*-* '(b)) 'reject)
(check-equal? (sm-apply ab*-* '(b b b)) 'reject)
(check-equal? (sm-apply ab*-* '()) 'accept)
(check-equal? (sm-apply ab*-* '(a a a a)) 'accept)
(check-equal? (sm-apply ab*-* '(a b a b b a b b b)) 'accept)
(check-equal? (sm-testequiv? ab*-* (sm-kleenestar ab*)) #t)


;; dfa --> dfa
;; Purpose: Construct a dfa for the complement of given dfa's language
(define (complement-fsa M)
  (let* [(new-finals (filter (λ (s) (not (member s (sm-finals M)))) (sm-states M)))]
    (make-dfa (sm-states M) (sm-sigma M) (sm-start M) new-finals (sm-rules M) 'no-dead)))

;; Tests for complement-fsa
(define not-a* (complement-fsa a*))
(define not-EVEN-A-ODD-B (complement-fsa EVEN-A-ODD-B))

(check-equal? (sm-apply not-a* '()) 'reject)
(check-equal? (sm-apply not-a* '(a a a)) 'reject)
(check-equal? (sm-apply not-a* '(a a b)) 'accept)
(check-equal? (sm-apply not-a* '(b b a a b)) 'accept)
(check-equal? (sm-testequiv? not-a* (sm-complement a*)) #t)

(check-equal? (sm-apply not-EVEN-A-ODD-B '(b)) 'reject)
(check-equal? (sm-apply not-EVEN-A-ODD-B '(a a b)) 'reject)
(check-equal? (sm-apply not-EVEN-A-ODD-B '(b b a b a)) 'reject)
(check-equal? (sm-apply not-EVEN-A-ODD-B '()) 'accept)
(check-equal? (sm-apply not-EVEN-A-ODD-B '(b b a a)) 'accept)
(check-equal? (sm-apply not-EVEN-A-ODD-B '(a a b b a b)) 'accept)
(check-equal? (sm-testequiv? not-EVEN-A-ODD-B (sm-complement EVEN-A-ODD-B)) #t)


;; ndfa ndfa --> ndfa
;; Purpose: Construct an ndfa for the intersection of the languages of the given ndfas
(define (intersect-fsa M N)
  (let* [(notM (complement-fsa (ndfa2dfa M)))
         (notN (complement-fsa (ndfa2dfa N)))]
    (complement-fsa (ndfa2dfa (union-fsa notM notN)))))

;; Tests for intersect-fsa
(define ab*-intersect-a-aUb-b* (intersect-fsa ab* a-aUb-b*))
(define a-aUb-b*-intersect-EVEN-A-ODD-B (intersect-fsa  a-aUb-b* EVEN-A-ODD-B))

(check-equal? (sm-apply ab*-intersect-a-aUb-b* '()) 'reject)
(check-equal? (sm-apply ab*-intersect-a-aUb-b* '(a b b a)) 'reject)
(check-equal? (sm-apply ab*-intersect-a-aUb-b* '(a b)) 'reject)
(check-equal? (sm-testequiv? ab*-intersect-a-aUb-b* (sm-intersection ab* a-aUb-b*)) #t)

(check-equal? (sm-apply a-aUb-b*-intersect-EVEN-A-ODD-B '()) 'reject)
(check-equal? (sm-apply a-aUb-b*-intersect-EVEN-A-ODD-B '(b b)) 'reject)
(check-equal? (sm-apply a-aUb-b*-intersect-EVEN-A-ODD-B '(a a b b)) 'reject)
(check-equal? (sm-apply a-aUb-b*-intersect-EVEN-A-ODD-B '(a a b)) 'accept)
(check-equal? (sm-apply a-aUb-b*-intersect-EVEN-A-ODD-B '(a a b b b)) 'accept)
(check-equal? (sm-testequiv? a-aUb-b*-intersect-EVEN-A-ODD-B
                             (sm-intersection a-aUb-b* EVEN-A-ODD-B))
              #t)