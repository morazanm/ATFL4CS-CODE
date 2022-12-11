#lang fsm

;; word word word (listof state) symbol --> dfa-rule
;; Purpose: Generate dfa fail rule for given state and given word for given pattern
(define (gen-state-tran s to-match patt states last-read)

  ;(define d (displayln (format "state: ~s\n to-match: ~s\n     patt: ~s\n" s to-match patt)))
  
  (cond [(empty? to-match) (list s last-read (first states))]
        [(> (length to-match) (length patt)) (list s last-read s)]
        [(equal? to-match (take patt (length to-match)))
         (list s (last to-match) (list-ref states (length to-match)))]
        [else (gen-state-tran s (rest to-match) patt states last-read)]))

;; Tests for gen-state-tran
(check-equal? (gen-state-tran 'C '(a b b b) '(a b b a b c) '(S A B C D E F) 'b)
              '(C b S))
(check-equal? (gen-state-tran 'S '(b) '(a b b a b c) '(S A B C D E F) 'b)
              '(S b S))
(check-equal? (gen-state-tran 'F '(a b b a b c a) '(a b b a b c) '(S A B C D E F) 'a)
              '(F a F))
(check-equal? (gen-state-tran 'S '(a) '(a b b a b c) '(S A B C D E F) 'a)
              '(S a A))
(check-equal? (gen-state-tran 'D '(a b b a c) '(a b b a b c) '(S A B C D E F) 'c)
              '(D c S))
(check-equal? (gen-state-tran 'E '(a b b a b b) '(a b b a b c) '(S A B C D E F) 'b)
              '(E b C))

;; state (listof states) alphabet word word --> (listof dfa-rule)
;; Purpose: Generate failed match transitions for the given state
(define (gen-state-trans s states sigma cp patt)

  ;(define d (displayln (format "state: ~s\n states: ~s\n sigma: ~s\n prefix: ~s\n pattern: ~s\n" s states sigma cp patt))) 
  
  (map (λ (a) (gen-state-tran s (append cp (list a)) patt states a))
       sigma))

;; Tests for gen-state-trans
(check-equal? (gen-state-trans 'E '(S A B C D E F) '(a b c) '(a b b a b) '(a b b a b c))
              '((E a A) (E b C) (E c F)))
(check-equal? (gen-state-trans 'S '(S A B C D E F) '(a b c) '() '(a b b a b c))
              '((S a A) (S b S) (S c S)))
(check-equal? (gen-state-trans 'S '(S A F) '(a b) '() '(a b))
              '((S a A) (S b S)))
(check-equal? (gen-state-trans 'A '(S A F) '(a b) '(a) '(a b))
              '((A a A) (A b F)))
(check-equal? (gen-state-trans 'F '(S A F) '(a b) '(a b) '(a b))
              '((F a F) (F b F)))
      

;; word alphabet --> dfa
;; Purpose: Build a dfa for L = all words that contain the given pattern
(define (build-pattern-dfa patt sigma)
  (let* [(sts (build-list (add1 (length patt))
                          (λ (n) (generate-symbol 'A '(A)))))
         (core-prefixes (build-list (add1 (length patt))
                                    (λ (i) (take patt i))))
         (deltas (append-map (λ (s cp) (gen-state-trans s sts sigma cp patt))
                             sts
                             core-prefixes))]
    (make-dfa sts sigma (first sts) (list (last sts)) deltas 'no-dead)))

;; Tests for build-pattern-dfa
(define M (build-pattern-dfa '(a b b a) '(a b)))
(define N (build-pattern-dfa '(a d) '(a b c d)))

(check-equal? (sm-apply M '()) 'reject)
(check-equal? (sm-apply M '(a a b b b a)) 'reject)
(check-equal? (sm-apply M '(b b b a a a b b)) 'reject)
(check-equal? (sm-apply M '(a b b a)) 'accept)
(check-equal? (sm-apply M '(b b a a a b b a b b a)) 'accept)
(check-equal? (sm-apply M '(a b b b a b b a)) 'accept)

(check-equal? (sm-apply N '()) 'reject)
(check-equal? (sm-apply N '(a b c d a b c c)) 'reject)
(check-equal? (sm-apply N '(c c b a b d)) 'reject)
(check-equal? (sm-apply N '(a d)) 'accept)
(check-equal? (sm-apply N '(b c a a d c c b)) 'accept)
(check-equal? (sm-apply N '(c d b c a d c a d)) 'accept)

;; word word alphabet --> Boolean
;; Purpose: Determine if the given first word is in the given second word
(define (contains-pattern? patt text sigma)
  (let [(M (build-pattern-dfa patt sigma))]
    (if (eq? (sm-apply M text) 'accept) #t #f)))

;; Tests for contains-pattern?
(check-equal? (contains-pattern? '(a b b a b c) '() '(a b c)) #f)
(check-equal? (contains-pattern? '(a b b a b c) '(a b c a a a b b a b b c b a a b) '(a b c)) #f)
(check-equal? (contains-pattern? '(a b b a b c) '(a b b a b c) '(a b c)) #t)
(check-equal? (contains-pattern? '(a b b a b c) '(a a b b a b a b b a b c a c c c c) '(a b c)) #t)

;; contains '(b a a)?
;;(define M2 (build-pattern-dfa '(b a a) '(a b)))

(check-equal? (contains-pattern? '(b a a) '() '(a b)) #f)
(check-equal? (contains-pattern? '(b a a) '(a b a b a b b a b b c b a b) '(a b)) #f)
(check-equal? (contains-pattern? '(b a a) '(b a a) '(a b)) #t)
(check-equal? (contains-pattern? '(b a a) '(a a b b a b a b b a a b a) '(a b)) #t)
