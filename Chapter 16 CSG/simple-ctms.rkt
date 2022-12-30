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

;; PRE: tape = (LM w), where w in (a b BLANK)*
;; POST: tape = (LM w)
(define HALT (make-tm '(S)
                      `(a b)
                      `()
                      'S
                      '(S)))

(check-equal? (last (sm-showtransitions HALT `(,LM ,BLANK) 1))
              `(S 1 (,LM _)))
(check-equal? (last (sm-showtransitions  HALT `(,LM a a b a b) 4))
              `(S 4 (,LM a a b a b)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (a b BLANK)* AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=a
(define Wa (make-tm '(S H)
                    `(a b ,LM)
                    `(((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S
                    '(H)))

(check-equal? (last (sm-showtransitions Wa `(,LM b) 1))
              `(H 1 (,LM a)))
(check-equal? (last (sm-showtransitions Wa `(,LM b a ,BLANK a) 3))
              `(H 3 (,LM b a a a)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (a b BLANK)* AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=b
(define Wb (make-tm '(S H)
                    `(a b ,LM)
                    `(((S a) (H b))
                      ((S b) (H b))
                      ((S ,BLANK) (H b)))
                    'S
                    '(H)))

(check-equal? (last (sm-showtransitions Wb `(,LM ,BLANK ,BLANK) 2))
              `(H 2 (,LM ,BLANK b)))
(check-equal? (last (sm-showtransitions Wb `(,LM b b b b) 3))
              `(H 3 (,LM b b b b)))

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



;;  PRE: tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
(define R^2 (make-tm '(S A F)
                     '(a b)
                     `(((S a) (A ,RIGHT))
                       ((S b) (A ,RIGHT))
                       ((S ,BLANK) (A ,RIGHT))
                       ((A a) (F ,RIGHT))
                       ((A b) (F ,RIGHT))
                       ((A ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

(check-equal? (last (sm-showtransitions R^2 `(,LM a b a) 1))
              `(F 3 (,LM a b a)))
(check-equal? (last (sm-showtransitions R^2 `(,LM a b a) 3))
              `(F 5 (,LM a b a ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions R^2 `(,LM b b a a) 4))
              `(F 6 (,LM b b a a ,BLANK ,BLANK)))


;;  PRE: tape = (LM BLANK i r^* BLANK w h v) AND head on first blank
;; POST: tape = (LM BLANK p i BLANK o) OR 
;;              (LM BLANK p i BLANK u) 
;;       AND head on second blank,
;; where p = r^n for a natnum n, 
;;       o = w'hv' where |w'|-|w| = n, 
;;       u = w'v'BLANK^*h where |w'v'BLANK^*|-|w| = n,
;;       w,w',v,v' in Σ^*, wv = w'v'
(define PR^N (make-tm '(S A B D E F G H I J K M N Q Y)
                      '(a b h i r)
                      `(((S ,BLANK) (A ,RIGHT)) ;; start the simulation
                        ((A i) (Q ,RIGHT)) ;; move to next instruction if any
                        ((Q ,BLANK) (Y ,BLANK)) ;; check if done
                        ((Q r) (M i)) ;; Otherwise swap i and r in previous position
                        ((M i) (N ,LEFT)) 
                        ((N i) (B r))
                        ((B r) (B ,RIGHT)) ;; find h
                        ((B i) (B ,RIGHT))
                        ((B ,BLANK) (B ,RIGHT))
                        ((B a) (B ,RIGHT))
                        ((B b) (B ,RIGHT))
                        ((B h) (D ,RIGHT))
                        ((D a) (E h)) ;; read a and move simultaed head right
                        ((D b) (G h)) ;; read b and move simultaed head right
                        ((D ,BLANK) (I h)) ;; read blank and move simultaed head right
                        ((E h) (F ,LEFT)) ;; move simulated head left after a read
                        ((F h) (K a))     ;; substitute h with a
                        ((G h) (H ,LEFT)) ;; move simulated head left after b read
                        ((H h) (K b))     ;; substitute h with b
                        ((I h) (J ,LEFT)) ;; move simulated head left after blank read
                        ((J h) (K ,BLANK)) ;; substitute h with blank
                        ((K a) (K ,LEFT)) ;; find i
                        ((K b) (K ,LEFT))
                        ((K ,BLANK) (K ,LEFT))
                        ((K r) (K ,LEFT))
                        ((K i) (Q ,RIGHT))) 
                      'S
                      '(Y)))
                        
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i ,BLANK h a a) 1))
              `(Y 3 (@ ,BLANK i ,BLANK h a a)))
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i r r ,BLANK a h b a) 1))
              `(Y 5 (,LM ,BLANK r r i ,BLANK a b a h)))
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i r r r ,BLANK h a b a) 1))
              `(Y 6 (@ ,BLANK r r r i ,BLANK a b a h)))
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i r r ,BLANK a h a b a) 1))
              `(Y 5 (@ ,BLANK r r i ,BLANK a a b h a)))
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i r r r ,BLANK h) 1))
              `(Y 6 (@ ,BLANK r r r i ,BLANK ,BLANK ,BLANK ,BLANK h)))
(check-equal? (last (sm-showtransitions PR^N `(,LM ,BLANK i r ,BLANK a h) 1))
              `(Y 4 (@ ,BLANK r i ,BLANK a ,BLANK h)))


;; CTMs

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
(define RR (combine-tms (list R R) '(a b)))

(check-equal? (ctm-run RR `(,LM b a a) 1) `(F 3 (,LM b a a)))
(check-equal? (ctm-run RR `(,LM a a a) 2) `(F 4 (,LM a a a ,BLANK)))
(check-equal? (ctm-run RR `(,LM a b b a) 3) `(F 5 (,LM a b b a ,BLANK)))
(check-equal? (ctm-run RR `(,LM b) 1) `(F 3 (,LM b ,BLANK ,BLANK)))

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


;; PRE: tape = (LM a1 a2 ... an) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM a1 ... ai ai ai ... ai+3 ... an) AND i=k+2
(define WTWICER (combine-tms `(((VAR s)
                               ,R
                               s
                               ,R
                               s))
                             (list 'a 'b LM)))

(check-equal? (ctm-run WTWICER `(,LM a ,BLANK) 1)
              `(h 3 (,LM a a a)))
(check-equal? (ctm-run WTWICER `(,LM b a a a ,BLANK) 1)
              `(h 3 (,LM b b b a _)))
(check-equal? (ctm-run WTWICER `(,LM a b ,BLANK ,BLANK a) 2)
              `(h 4 (,LM a b b b a)))