#lang fsm

;;  PRE: tape = (LMw) AND i=k>0 AND w in (a b)*
;; POST: tape = (LMw) AND i=k+1 AND w in (a b)*
(define R (make-tm '(S F)
                   '(a b)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

(check-equal? (sm-showtransitions R `(,LM a b a) 1)
              '((S 1 (@ a b a))
                (F 2 (@ a b a))))
(check-equal? (sm-showtransitions R `(,LM a b a) 3)
              `((S 3 (@ a b a))
                (F 4 (@ a b a ,BLANK))))
(check-equal? (second (last (sm-showtransitions R `(,LM b b a a) 3)))
              4)

;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
;; POST: tape = (LMw) AND i=k+1
(define L (make-tm '(S H)
                   `(a b ,LM)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

(check-equal? (last (sm-showtransitions L `(,LM a a) 1))
              `(H 0 (,LM a a)))
(check-equal? (last (sm-showtransitions L `(,LM ,BLANK b b a) 4))
              `(H 3 (,LM ,BLANK b b a)))

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
(define RR (combine-tms (list R R) '(a b)))

(check-equal? (ctm-run RR `(,LM b a a) 1) `(F 3 (,LM b a a)))
(check-equal? (ctm-run RR `(,LM a a a) 2) `(F 4 (,LM a a a ,BLANK)))
(check-equal? (ctm-run RR `(,LM a b b a) 3) `(F 5 (,LM a b b a ,BLANK)))
(check-equal? (ctm-run RR `(,LM b) 1) `(F 3 (,LM b ,BLANK ,BLANK)))


;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)* AND exists j<i s.t. tape[j]=BLANK
;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
(define FBL (combine-tms (list 0 L (cons BRANCH
                                         (list (list 'a (list GOTO 0))
                                               (list 'b (list GOTO 0))
                                               (list LM (list GOTO 0))
                                               (list BLANK ))))
                         (list 'a 'b LM)))

(check-equal? (ctm-run FBL `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run FBL `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))
;; Infinite recursion, PRE not met
;;    (ctm-run FBL `(,LM a a b) 3)

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i>k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
(define FBR (combine-tms (list 0 R (cons BRANCH
                                         (list (list 'a (list GOTO 0))
                                               (list 'b (list GOTO 0))
                                               (list LM (list GOTO 0))
                                               (list BLANK ))))
                         (list 'a 'b LM)))

(check-equal? (ctm-run FBR `(,LM ,BLANK) 1)
              `(F 2 (,LM ,BLANK ,BLANK)))
(check-equal? (ctm-run FBR `(,LM a a b b b ,BLANK a a) 2)
              `(F 6 (,LM a a b b b ,BLANK a a)))
(check-equal? (ctm-run FBR `(,LM ,BLANK ,BLANK b b) 3)
              `(F 5 (,LM ,BLANK ,BLANK b b ,BLANK)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (a b BLANK)* AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
(define WB (make-tm '(S H)
                    `(a b ,LM)
                    `(((S a) (H ,BLANK))
                      ((S b) (H ,BLANK))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))

(check-equal? (last (sm-showtransitions WB `(,LM a a) 1))
              `(H 1 (,LM ,BLANK a)))
(check-equal? (last (sm-showtransitions WB `(,LM a b b b) 3))
              `(H 3 (,LM a b ,BLANK b)))

;;  PRE: tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w
(define COPY (combine-tms
              (list FBL 
                    0 
                    R 
                    (cons BRANCH (list (list BLANK (list GOTO 2))                                                                
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))))
                    1
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))))
                    3
                    RR
                    (list GOTO 5)
                    4
                    R
                    (list GOTO 5)
                    5)
              `(a b)))

(check-equal? (rest (ctm-run COPY `(,LM ,BLANK ,BLANK ,BLANK) 3))
              `(5 (,LM  ,BLANK ,BLANK ,BLANK ,BLANK ,BLANK)))
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK a b b ,BLANK) 5))
              `(9 (,LM ,BLANK a b b ,BLANK a b b ,BLANK)))
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK b b b b ,BLANK) 5))
              `(11 (,LM ,BLANK b b b b ,BLANK b b b b ,BLANK)))
