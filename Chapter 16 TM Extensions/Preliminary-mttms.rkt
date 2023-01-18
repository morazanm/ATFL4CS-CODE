#lang fsm

; 1 tape
(define M1 (make-mttm '(S Y N)
                      `(a b ,LM)
                      'S
                      '(Y N)
                      #;'(((S (a)) (S (R)))
                          ((S (b)) (N (b)))
                          ((S (_)) (Y (_))))
                      (list (list (list 'S (list 'a))
                                  (list 'S (list RIGHT)))
                            (list (list 'S (list 'b))
                                  (list 'N (list 'b)))
                            (list (list 'S (list BLANK))
                                  (list 'Y (list BLANK))))
                      1
                      'Y))
;; L = a*
;; two tapes
(define M2 (make-mttm '(S Y N)
                      `(a b ,LM)
                      'S
                      '(Y N)
                      '(((S (a _)) (S (R R)))
                        ((S (b _)) (N (b _)))
                        ((S (_ _)) (Y (_ _))))
                      2
                      'Y))

;; Add (ok, this is a dumb way to do add but for testing it's great)
;; three tapes
;; PRE: _A_B T1 head starts at position 0 (blank before A)
;;      _
;;      _
;; POST: A+B (on T1)
;; HOW: Copy A to T2, Copy B to T3, COPY T2 and T3 to T1
(define ADD (make-mttm '(S A T U V)
                       `(I)
                       'S
                       '(H)
                       `(((S (,BLANK ,BLANK ,BLANK)) (A (R R R)))
                         ((A (I ,BLANK ,BLANK)) (A (,BLANK I ,BLANK)))
                         ((A (,BLANK I ,BLANK)) (A (R R ,BLANK)))
                         ((A (,BLANK ,BLANK ,BLANK)) (T (R ,BLANK ,BLANK)))
                         ((T (I ,BLANK ,BLANK)) (T (,BLANK ,BLANK I)))
                         ((T (,BLANK ,BLANK I)) (T (R ,BLANK R)))
                         ((T (,BLANK ,BLANK ,BLANK)) (Q (L ,BLANK ,BLANK)))
                         ((Q (,BLANK ,BLANK ,BLANK)) (U (L L L)))
                         ((U (,BLANK I I)) (U (I I ,BLANK)))
                         ((U (I I ,BLANK)) (U (L I L)))
                         ((U (,BLANK I ,BLANK)) (V (,BLANK I ,BLANK)))
                         ((V (,BLANK I ,BLANK)) (V (I ,BLANK ,BLANK)))
                         ((V (I ,BLANK ,BLANK)) (V (L L ,BLANK)))
                         ((V (,BLANK ,BLANK ,BLANK)) (H (,BLANK ,BLANK ,BLANK))))
                       3))

;; a^nb^nc^nd^n
;; four tapes
;; PRE: _w_ T1 head starts at position 0 (blank before w)
;;      _
;;      _
;;      _
;; POST: Y for accept and N for reject
;; HOW: Skip a's; copy b's, c's, d's to T2, T3, and T4. move all heaps to
;;      starting blank, match a's, b's, c's, and d's
(define a^nb^nc^nd^n (make-mttm
                      '(S A Y N)
                      '(a b c d)
                      'S
                      '(Y N)
                      `(;; Move to pos 1 on all heads
                        ((S (,BLANK ,BLANK ,BLANK ,BLANK)) (Q (R R R R)))
                        ;; check for empty
                        ((Q (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((Q (a ,BLANK ,BLANK ,BLANK)) (A (a ,BLANK ,BLANK ,BLANK)))
                        ;; no a's reject
                        ((Q (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ((Q (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ((Q (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Skip the a's on T1
                        ((A (a ,BLANK ,BLANK ,BLANK)) (A (R ,BLANK ,BLANK ,BLANK)))
                        ((A (b ,BLANK ,BLANK ,BLANK)) (B (b ,BLANK ,BLANK ,BLANK)))
                        ;; No b's reject
                        ((A (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((A (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ((A (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the b's on T1 to T2
                        ((B (b ,BLANK ,BLANK ,BLANK)) (B (b b ,BLANK ,BLANK)))
                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((B (b b ,BLANK ,BLANK)) (B (R R ,BLANK ,BLANK)))
                        ((B (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK ,BLANK ,BLANK)))
                        ;; No c's reject
                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((B (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((B (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the c's on T1 to T3
                        ((C (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK c ,BLANK)))
                        ((C (c ,BLANK c ,BLANK)) (C (R ,BLANK R ,BLANK)))
                        ((C (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK ,BLANK)))
                        ;; No d's reject
                        ((C (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((C (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((C (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the d's on T1 to T4
                        ((D (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK d)))
                        ((D (d ,BLANK ,BLANK d)) (D (R ,BLANK ,BLANK R)))
                        ((D (,BLANK ,BLANK ,BLANK ,BLANK)) (E (L L L L)))
                        ;; non d reject
                        ((D (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((D (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ((D (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ;; Match a's, b's, c's, and d's to accept. Otherwise, reject if BLANK read and num of BLANK is < 4
                        ((E (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK))) ;; accept
                        ((E (d b c d)) (E (L b c d))) ;; skip d's on T1
                        ((E (c b c d)) (E (L b c d))) ;; skip c's on T1 
                        ((E (b b c d)) (E (L b c d))) ;; skip c's on T1
                        ((E (a b c d)) (E (L L L L))) ;; on match move all heads L
                        ;; Wrong number of blanks reject
                        ((E (,BLANK b c d)) (N (,BLANK b c d)))
                        ((E (a ,BLANK c d)) (N (a ,BLANK c d)))
                        ((E (a b ,BLANK d)) (N (a b ,BLANK d)))
                        ((E (a b c ,BLANK)) (N (a b c ,BLANK)))
                        ((E (,BLANK ,BLANK c d)) (N (,BLANK ,BLANK c d)))
                        ((E (,BLANK b ,BLANK d)) (N (,BLANK b ,BLANK d)))
                        ((E (,BLANK b c ,BLANK)) (N (,BLANK b c ,BLANK)))
                        ((E (a ,BLANK ,BLANK d)) (N (a ,BLANK ,BLANK d)))
                        ((E (a ,BLANK c ,BLANK)) (N (a ,BLANK c ,BLANK)))
                        ((E (a b ,BLANK ,BLANK)) (N (a b ,BLANK ,BLANK)))
                        ((E (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((E (,BLANK b ,BLANK ,BLANK)) (N (,BLANK b ,BLANK ,BLANK)))
                        ((E (,BLANK ,BLANK c ,BLANK)) (N (,BLANK ,BLANK c ,BLANK)))
                        ((E (,BLANK ,BLANK ,BLANK d)) (N (,BLANK ,BLANK ,BLANK d))))
                      4
                      'Y))

                        
                      


;(M1-transitions '(a a a) 'S '(Y N) '(((S (a)) (S (R))) ((S (b)) (N (b))) ((S (_)) (Y (_)))) 'Y)

(check-equal? (sm-states M1) '(S Y N))
(check-equal? (sm-sigma M1) '(a b))
(check-equal? (sm-start M1) 'S)
(check-equal? (sm-finals M1) '(Y N))
(check-equal? (sm-accept M1) 'Y)
(check-equal? (sm-rules M2)
              '(((S (a _)) (S (R R)))
                ((S (b _)) (N (b _)))
                ((S (_ _)) (Y (_ _)))))
(check-equal? (sm-type M2) 'mttm-language-recognizer)
(check-equal? (sm-apply M2 `(,LM a a a a) 1) 'accept)
(check-equal? (sm-apply M2 `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply M2 `(,LM a a b a a) 1) 'reject)
(check-equal? (sm-showtransitions M2 `(,LM a a a) 1)
              `((S (1 (,LM a a a)) (0 (_)))
                (S (2 (,LM a a a)) (1 (_ _)))
                (S (3 (,LM a a a)) (2 (_ _ _)))
                (S (4 (,LM a a a _)) (3 (_ _ _ _)))
                (Y (4 (,LM a a a _)) (3 (_ _ _ _)))
                accept))
(check-equal? (sm-showtransitions M2 `(,LM a a a b a) 1)
              `((S (1 (,LM a a a b a)) (0 (_)))
                (S (2 (,LM a a a b a)) (1 (_ _)))
                (S (3 (,LM a a a b a)) (2 (_ _ _)))
                (S (4 (,LM a a a b a)) (3 (_ _ _ _)))
                (N (4 (,LM a a a b a)) (3 (_ _ _ _)))
                reject))

(check-equal? (sm-showtransitions M2 '())
              '((S (0 (_)) (0 (_)))
                (Y (0 (_)) (0 (_)))
                accept))

(check-equal? (sm-apply ADD `(,BLANK I I I ,BLANK I I))
              '(H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _))))

(check-equal? (sm-showtransitions ADD `(,BLANK I I I ,BLANK I I))
              '((S (0 (_ I I I _ I I)) (0 (_)) (0 (_)))
                (A (1 (_ I I I _ I I)) (1 (_ _)) (1 (_ _)))
                (A (1 (_ _ I I _ I I)) (1 (_ I)) (1 (_ _)))
                (A (2 (_ _ I I _ I I)) (2 (_ I _)) (1 (_ _)))
                (A (2 (_ _ _ I _ I I)) (2 (_ I I)) (1 (_ _)))
                (A (3 (_ _ _ I _ I I)) (3 (_ I I _)) (1 (_ _)))
                (A (3 (_ _ _ _ _ I I)) (3 (_ I I I)) (1 (_ _)))
                (A (4 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
                (T (5 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
                (T (5 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (1 (_ I)))
                (T (6 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (2 (_ I _)))
                (T (6 (_ _ _ _ _ _ _)) (4 (_ I I I _)) (2 (_ I I)))
                (T (7 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
                (Q (6 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
                (U (5 (_ _ _ _ _ _ _ _)) (3 (_ I I I _)) (2 (_ I I _)))
                (U (5 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (2 (_ I _ _)))
                (U (4 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (1 (_ I _ _)))
                (U (4 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (1 (_ _ _ _)))
                (U (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
                (V (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
                (V (3 (_ _ _ I I I _ _)) (3 (_ I I _ _)) (0 (_ _ _ _)))
                (V (2 (_ _ _ I I I _ _)) (2 (_ I I _ _)) (0 (_ _ _ _)))
                (V (2 (_ _ I I I I _ _)) (2 (_ I _ _ _)) (0 (_ _ _ _)))
                (V (1 (_ _ I I I I _ _)) (1 (_ I _ _ _)) (0 (_ _ _ _)))
                (V (1 (_ I I I I I _ _)) (1 (_ _ _ _ _)) (0 (_ _ _ _)))
                (V (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))
                (H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))))

(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a a b b c c d d)) 'accept)
(check-equal? (sm-showtransitions a^nb^nc^nd^n `(,BLANK a a b b c c d d))
              '((S (0 (_ a a b b c c d d)) (0 (_)) (0 (_)) (0 (_)))
                (Q (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (2 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (3 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (B (3 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (B (3 (_ a a b b c c d d)) (1 (_ b)) (1 (_ _)) (1 (_ _)))
                (B (4 (_ a a b b c c d d)) (2 (_ b _)) (1 (_ _)) (1 (_ _)))
                (B (4 (_ a a b b c c d d)) (2 (_ b b)) (1 (_ _)) (1 (_ _)))
                (B (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
                (C (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
                (C (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ c)) (1 (_ _)))
                (C (6 (_ a a b b c c d d)) (3 (_ b b _)) (2 (_ c _)) (1 (_ _)))
                (C (6 (_ a a b b c c d d)) (3 (_ b b _)) (2 (_ c c)) (1 (_ _)))
                (C (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ _)))
                (D (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ _)))
                (D (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ d)))
                (D (8 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (2 (_ d _)))
                (D (8 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (2 (_ d d)))
                (D (9 (_ a a b b c c d d _)) (3 (_ b b _)) (3 (_ c c _)) (3 (_ d d _)))
                (E (8 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (7 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (6 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (5 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (4 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (3 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (2 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                (E (1 (_ a a b b c c d d _)) (1 (_ b b _)) (1 (_ c c _)) (1 (_ d d _)))
                (E (0 (_ a a b b c c d d _)) (0 (_ b b _)) (0 (_ c c _)) (0 (_ d d _)))
                (Y (0 (_ a a b b c c d d _)) (0 (_ b b _)) (0 (_ c c _)) (0 (_ d d _)))
                accept))

(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a a a a b b b b c c c c d d d))
              'reject)

(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK b b b c c c d d d))
              'reject)

(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a a b c d d))
              'reject)

(check-equal? (sm-showtransitions a^nb^nc^nd^n `(,BLANK a a a a b b b b c c c c d d d))
              '((S (0 (_ a a a a b b b b c c c c d d d)) (0 (_)) (0 (_)) (0 (_)))
                (Q (1 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (1 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (2 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (3 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (4 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (A (5 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (B (5 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (B (5 (_ a a a a b b b b c c c c d d d)) (1 (_ b)) (1 (_ _)) (1 (_ _)))
                (B (6 (_ a a a a b b b b c c c c d d d)) (2 (_ b _)) (1 (_ _)) (1 (_ _)))
                (B (6 (_ a a a a b b b b c c c c d d d)) (2 (_ b b)) (1 (_ _)) (1 (_ _)))
                (B (7 (_ a a a a b b b b c c c c d d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
                (B (7 (_ a a a a b b b b c c c c d d d)) (3 (_ b b b)) (1 (_ _)) (1 (_ _)))
                (B (8 (_ a a a a b b b b c c c c d d d)) (4 (_ b b b _)) (1 (_ _)) (1 (_ _)))
                (B (8 (_ a a a a b b b b c c c c d d d)) (4 (_ b b b b)) (1 (_ _)) (1 (_ _)))
                (B (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ _)) (1 (_ _)))
                (C (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ _)) (1 (_ _)))
                (C (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ c)) (1 (_ _)))
                (C (10 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (2 (_ c _)) (1 (_ _)))
                (C (10 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (2 (_ c c)) (1 (_ _)))
                (C (11 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (3 (_ c c _)) (1 (_ _)))
                (C (11 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (3 (_ c c c)) (1 (_ _)))
                (C (12 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (4 (_ c c c _)) (1 (_ _)))
                (C (12 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (4 (_ c c c c)) (1 (_ _)))
                (C (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ _)))
                (D (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ _)))
                (D (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ d)))
                (D (14 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (2 (_ d _)))
                (D (14 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (2 (_ d d)))
                (D (15 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (3 (_ d d _)))
                (D (15 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (3 (_ d d d)))
                (D
                 (16 (_ a a a a b b b b c c c c d d d _))
                 (5 (_ b b b b _))
                 (5 (_ c c c c _))
                 (4 (_ d d d _)))
                (E
                 (15 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E
                 (14 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E
                 (13 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E
                 (12 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E
                 (11 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E
                 (10 (_ a a a a b b b b c c c c d d d _))
                 (4 (_ b b b b _))
                 (4 (_ c c c c _))
                 (3 (_ d d d _)))
                (E (9 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (8 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (7 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (6 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (5 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (4 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
                (E (3 (_ a a a a b b b b c c c c d d d _)) (3 (_ b b b b _)) (3 (_ c c c c _)) (2 (_ d d d _)))
                (E (2 (_ a a a a b b b b c c c c d d d _)) (2 (_ b b b b _)) (2 (_ c c c c _)) (1 (_ d d d _)))
                (E (1 (_ a a a a b b b b c c c c d d d _)) (1 (_ b b b b _)) (1 (_ c c c c _)) (0 (_ d d d _)))
                (N (1 (_ a a a a b b b b c c c c d d d _)) (1 (_ b b b b _)) (1 (_ c c c c _)) (0 (_ d d d _)))
                reject))

(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a b c)) 'reject)
(check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a b c d b)) 'reject)
(check-equal? (sm-showtransitions a^nb^nc^nd^n '())
              '((S (0 (_)) (0 (_)) (0 (_)) (0 (_)))
                (Q (1 (_ _)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                (Y (1 (_ _)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                accept))
