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

(define NO-ABAA (make-dfa '(S A B C R)
                          '(a b)
                          'S
                          '(S A B C)
                          '((S a A)
                            (S b S)
                            (A a A)
                            (A b B)
                            (B a C)
                            (B b S)
                            (C a R)
                            (C b B)
                            (R a R)
                            (R b R))
                          'no-dead))

(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

;; A stack element, stacke, is either
;;  1. EMP
;;  2. (listof symbol)
     
;; A super state, ss, is a (list state state)
     
;; A pda super rule, ssrule, is (list (list ss symbol stacke) (list ss stacke))

;; pda dfa --> pda
;; Purpose: Build a pda for the intersection of the languages of the given machines
(define (pda-intersect-dfa a-pda a-dfa)
  ;; (listof state) (listof state) --> (listof ss)
  ;; Purpose: Create a list of super states containing the Cartesian product of the given states
  (define (cartesian-product pda-sts dfa-sts)
    (for*/list [(s1 pda-sts)
                (s2 dfa-sts)]
      (list s1 s2)))

  ;; (listof pda-rule) (listof state) --> (listof ssrule)
  ;; Purpose: Create ssrules for given empty transition pda-rules
  (define (make-EMP-rls EMP-pda-rls dfa-sts)
    (for*/list [(r EMP-pda-rls)
                (s dfa-sts)]
      (list (list (list (first (first r)) s) EMP (third (first r)))
            (list (list (first (second r)) s) (second (second r))))))

  ;; (listof pda-rule) (listof state) (listof dfa-rls) --> (listof ssrule)
  ;; Purpose: Create ssrules for given nonempty transition pda-rules
  (define (make-nonEMP-rls non-EMP-pda-rls dfa-sts dfa-rls)
    (for*/list [(r non-EMP-pda-rls)
                (s dfa-sts)]
      (list (list (list (first (first r)) s)
                  (second (first r))
                  (third (first r)))
            (list (list (first (second r))
                        (third (first
                                (filter (λ (rl) (and (eq? (first rl) s)
                                                     (eq? (second rl) (second (first r)))))
                                        dfa-rls))))
                  (second (second r))))))

  ;; (listof ss) --> (listof (list ss state))
  ;; Purpose: Create table associating given super states with a new state
  (define (make-ss-table K)
    (for*/list [(ss K)]
      (list ss (generate-symbol 'T '(T)))))

  ;; (listof ssrule) (ss --> state) --> (listof pda-rule)
  ;; Purpose: Convert ssrules to pda-rules
  (define (convert-ssrules ss-rls ss->state)
    (for*/list [(r ss-rls)]
      (list (list (ss->state (first (first r)))
                  (second (first r))
                  (third (first r)))
            (list (ss->state (first (second r)))
                  (second (second r))))))
  
  (let* [(pda-sts (sm-states a-pda))
         (dfa-sts (sm-states a-dfa))
         (pda-rls (sm-rules a-pda))
         (dfa-rls (sm-rules a-dfa))
         (K (cartesian-product pda-sts dfa-sts))
         (start (list (sm-start a-pda) (sm-start a-dfa)))
         (F (cartesian-product (sm-finals a-pda) (sm-finals a-dfa)))
         (non-EMP-pda-rls (filter (λ (r) (not (eq? (second (first r)) EMP)))
                                  pda-rls))
         (EMP-pda-rls (filter (λ (r) (eq? (second (first r)) EMP))
                              pda-rls))
         
         (non-EMP-rls (make-nonEMP-rls non-EMP-pda-rls dfa-sts dfa-rls))
         (EMP-rls (make-EMP-rls EMP-pda-rls dfa-sts))
         (ss-tbl (make-ss-table K))
         (ss->state (λ (st) (second (assoc st ss-tbl))))]
    (make-ndpda (map ss->state K)
                (sm-sigma a-pda)
                (sm-gamma a-pda)
                (ss->state start)
                (map ss->state F)
                (convert-ssrules (append non-EMP-rls EMP-rls) ss->state))))

;; Tests for pda-intersect-dfa
(define a^nb^nIM (pda-intersect-dfa a^nb^n M))
(define a^nb^nINO-ABBA (pda-intersect-dfa a^nb^n NO-ABAA))

(check-equal? (sm-apply a^nb^nIM '()) 'reject)
(check-equal? (sm-apply a^nb^nIM '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^nIM '(a a b b)) 'reject)
(check-equal? (sm-apply a^nb^nIM '(a b)) 'accept)

(check-equal? (sm-apply a^nb^nINO-ABBA '(b b)) 'reject)
(check-equal? (sm-apply a^nb^nINO-ABBA '(a a b)) 'reject)
(check-equal? (sm-apply a^nb^nINO-ABBA '()) 'accept)
(check-equal? (sm-apply a^nb^nINO-ABBA '(a a b b)) 'accept)
