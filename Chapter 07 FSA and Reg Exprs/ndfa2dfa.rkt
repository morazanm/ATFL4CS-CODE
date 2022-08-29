#lang racket

(require fsm rackunit)

(provide ndfa2dfa)

;; Data Definitions
;;
;; An ndfa transition rule, ndfa-rule, is a (list state symbol state)
;; A super state, ss, is a (listof state)
;; A ss-dfa-rule is a (list ss symbol ss)
;; An empties table, emps-tbl, is a (listof (list state ss))
;; A super state name table, ss-name-table, is a (listof (list state ss))

;; state emps-tbl --> ss
;; Purpose: Extract the empties of the given state
;; Assume: Given state is in the given list of states
(define (extract-empties st empties)
  (second (first (filter (λ (e) (eq? (first e) st)) empties))))

;; (listof ss) alphabet emps-tbl (listof ndfa-rule) (listof ss) --> (listof ss-dfa-rule)
;; Purpose: Compute the dfa rules
;; Accumulator Invariants
;;   to-search-ssts = the super states that must still be explored
;;             ssts = the super states explored
(define (compute-ss-dfa-rules to-search-ssts sigma empties rules ssts)
  ;; state symbol (listof ndfa-rule) emps-tbl --> ss
  ;; Purpose: Find the reachable super state from the given state and the given alphabet element
  (define (find-reachables-from-st-on-a st a rules empties)
    (let* [(rls (filter (λ (r) (and (eq? (first r) st) (eq? (second r) a)))
                        rules))
           (to-states (map third rls))]
      (remove-duplicates (append-map (λ (st) (extract-empties st empties)) to-states))))

  ;; state alphabet (listof ndfa-rule) emps-tbl --> (listof ss)
  ;; Purpose: Find the reachable super state from the given state for each element of the given alphabet
  (define (find-reachables-from-st st sigma rules empties)
    (map (λ (a) (find-reachables-from-st-on-a st a rules empties))
         sigma))

  ;; ss alphabet (listof ndfa-rule) emps-tbl --> (listof (listof ss))
  ;; Purpose: Compute reachable super states from given super state
  (define (find-reachables ss sigma rules empties)
    (map (λ (st) (find-reachables-from-st st sigma rules empties)) ss))

  ;; natnum (listof (listof ss)) --> (listof ss)
  ;; Purpose: Return ss of ith (listof state) in each given list element
  (define (get-reachable i reachables)
    (remove-duplicates (append-map (λ (reached) (list-ref reached i))
                                   reachables)))
  (if (empty? to-search-ssts)
      '()
      (let* [(curr-ss (first to-search-ssts))
             (reachables (find-reachables curr-ss sigma rules empties))
             (to-super-states (build-list (length sigma)
                                          (λ (i) (get-reachable i reachables))))
             (new-rules (map (λ (sst a) (list curr-ss a sst))
                             to-super-states
                             sigma))]
        (append new-rules (compute-ss-dfa-rules
                           (append (rest to-search-ssts)
                                   (filter (λ (ss) (not (member ss (append to-search-ssts ssts))))
                                           to-super-states))
                           sigma
                           empties
                           rules
                           (cons curr-ss ssts))))))

;; (listof state) rules --> emps-tbl
;; Purpose: Compute empties table for all given states
(define (compute-empties-tbl states rules)
  ;; state (listof state) (listof ndfa-rule) --> (listof ndfa-rule)
  ;; Purpose: Extract empty transitions to non-generated states for the given state
  (define (get-e-trans state gen-states rules)
    (filter (λ (r) (and (eq? (first r) state)
                        (eq? (second r) EMP)
                        (not (member (third r) gen-states))))
            rules))
  
  ;; (listof state) (listof ndfa-rules) (listof state) --> (listof state)
  ;; Purpose: Compute the empties for the states left to explore in the first given (listof state)
  ;; Accumlator Invariants:
  ;;     to-search = states reachable by consuming no input that have not been visited
  ;;       visited = states reachable by consuming no input
  (define (compute-empties to-search rules visited)
    (if (empty? to-search)
        visited
        (let* [(curr (first to-search))
               (curr-e-rules (get-e-trans curr (append to-search visited) rules))]
          (compute-empties (append (rest to-search) (map third curr-e-rules))
                           rules
                           (cons curr visited)))))
  (map (λ (st) (list st (compute-empties (list st) rules '()))) states))

;; Tests for compute-empties-tbl
(check-equal? (compute-empties-tbl '(X Y Z)
                                   `((X ,EMP Y)
                                     (Y a Z)
                                     (Z ,EMP X)))
              '((X (Y X)) (Y (Y)) (Z (Y X Z))))
(check-equal? (compute-empties-tbl '(W X Y Z)
                                   `((W ,EMP X)
                                     (X ,EMP Y)
                                     (Y a Z)
                                     (Z ,EMP Y)
                                     (Z b Z)))
              '((W (Y X W)) (X (Y X)) (Y (Y)) (Z (Y Z))))
              
;; (listof ss) --> ss-name-tbl
;; Purpose: Create a table for ss names
(define (compute-ss-name-tbl super-states)
  (map (λ (ss) (list ss (generate-symbol 'X '(X))))                                      
       super-states))

;; Tests for compute-ss-name-tbl
(check-pred (lambda (tbl)
              (and (list? tbl)
                   (andmap (λ (e) (= (length e) 2)) tbl)
                   (andmap (λ (e) (andmap symbol? (first e))) tbl)
                   (andmap (λ (e) (symbol? (second e))) tbl)))
            (compute-ss-name-tbl '()))

(check-pred (lambda (tbl)
              (and (list? tbl)
                   (andmap (λ (e) (= (length e) 2)) tbl)
                   (andmap (λ (e) (andmap symbol? (first e))) tbl)
                   (andmap (λ (e) (symbol? (second e))) tbl)))
            (compute-ss-name-tbl '((A B) (A B C) () (C))))

;; (listof state) alphabet state (listof state) (list-of ndfa-rule) --> dfa
;; Purpose: Create a dfa from the given ndfa components
(define (convert states sigma start finals rules)
  (let* [(empties (compute-empties-tbl states rules))
         (ss-dfa-rules 
          (compute-ss-dfa-rules (list (extract-empties start empties))
                                sigma
                                empties
                                rules
                                '()))
         (super-states (remove-duplicates
                        (append-map
                         (λ (r) (list (first r) (third r)))
                         ss-dfa-rules)))
         (ss-name-tbl (compute-ss-name-tbl super-states))]
    (make-dfa (map (λ (ss) (second (assoc ss ss-name-tbl)))
                   super-states)
              sigma
              (second (assoc (first super-states) ss-name-tbl))
              (map (λ (ss) (second (assoc ss ss-name-tbl)))
                   (filter (λ (ss) (ormap (λ (s) (member s finals)) ss))
                           super-states))
              (map (λ (r) (list (second (assoc (first r) ss-name-tbl))
                                (second r)
                                (second (assoc (third r) ss-name-tbl))))
                   ss-dfa-rules)
              'no-dead)))

;; Tests for convert
(check-equal?
 (sm-testequiv?
  (convert '(S A B) '(a b) 'S '(A B) '((S a A)
                                       (S a B)
                                       (A a A)
                                       (B b B)))
  (make-ndfa '(S A B)
             '(a b)
             'S
             '(A B)
             '((S a A)
               (S a B)
               (A a A)
               (B b B)))
  500)
 #t)

(check-equal?
 (sm-testequiv?
  (convert '(S A) '(a b) 'S '(S A) '((S a S)
                                     (S a A)
                                     (A b A)
                                     (A a A)))
  (make-ndfa '(S A)
             '(a b)
             'S
             '(S A)
             '((S a S)
               (S a A)
               (A b A)
               (A a A)))
  500)
 #t)

;; ndfa --> dfa
;; Convert the given ndfa to an equivalent dfa
(define (ndfa2dfa M)
  (if (eq? (sm-type M) 'dfa)
      M
      (convert (sm-states M)
               (sm-sigma M)
               (sm-start M)
               (sm-finals M)
               (sm-rules M))))

;; L = {w | a not in w OR b not in w OR c not in w}
(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C)
                                        '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))

;; Tests for ndfa2dfa
(define M (ndfa2dfa AT-LEAST-ONE-MISSING))

(check-equal? (sm-testequiv? AT-LEAST-ONE-MISSING M 500) #t)
(check-equal? (sm-testequiv? M
                             (ndfa->dfa AT-LEAST-ONE-MISSING)
                             500)
              #t)

;; L = (aba)* U (ab)*
(define ND (make-ndfa '(S A B C D E)
                      '(a b)
                      'S
                      '(S)
                      `((S a A)
                        (S a B)
                        (A b C)
                        (B b D)
                        (C a E)
                        (D ,EMP S)
                        (E ,EMP S))))

(define N (ndfa2dfa ND))

(check-equal? (sm-testequiv? ND N 500) #t)
(check-equal? (sm-testequiv? N (ndfa->dfa ND) 500) #t)


