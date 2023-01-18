#lang fsm

;; DESIGN IDEA
;; Need 4 tapes: input, as, bs, cs
;; Start with (LM BLANK w) and i = 1 on tape 0
;; Copy as to tape 1, bs to tape 2, and cs to tape 3
;; Match a, b, c on tapes 1-3 and move heads 1, 2, 3 left
;;  if mismatch reject
;;  if BLANK on tapes 1-3 accept

;; State Documentation
;; S: tape 0 = (LM BLANK w) AND t0h = 1
;;    tape 1-3 = () AND t1h = t2h = t3h = 0
;;
;; C: tape 0 = (LM BLANK w) AND t0h = k >= 2
;;    tape 1 = (BLANK a* BLANK) AND num a = num a in tape1[2..k-1] AND t1h = num a in tape1[2..k-1] + 1 AND tape1[t1h] = BLANK 
;;    tape 2 = (BLANK b* BLANK) AND num b = num b in tape2[2..k-1] AND t2h = num b in tape1[2..k-1] + 1 AND tape1[t2h] = BLANK
;;    tape 3 = (BLANK c* BLANK) AND num c = num c in tape3[2..k-1] AND t3h = num c in tape1[2..k-1] + 1 AND tape1[t3h] = BLANK
;;
;; D: tape 0 = (LM BLANK w) AND t0h = k >= 2
;;    tape 1 = (BLANK a* BLANK) AND num a = num a in tape1[2..k] AND t1h = num a in tape1[2..k] + 1 AND tape1[t1h] = a 
;;    tape 2 = (BLANK b* BLANK) AND num b = num b in tape2[2..k] AND t2h = num b in tape1[2..k] + 1 AND tape1[t2h] = BLANK
;;    tape 3 = (BLANK c* BLANK) AND num c = num c in tape3[2..k] AND t3h = num c in tape1[2..k] + 1 AND tape1[t3h] = BLANK
;;
;; E: tape 0 = (LM BLANK w) AND t0h = k >= 2
;;    tape 1 = (BLANK a* BLANK) AND num a = num a in tape1[2..k] AND t1h = num a in tape1[2..k] + 1 AND tape1[t1h] = BLANK
;;    tape 2 = (BLANK b* BLANK) AND num b = num b in tape2[2..k] AND t2h = num b in tape1[2..k] + 1 AND tape1[t2h] = b
;;    tape 3 = (BLANK c* BLANK) AND num c = num c in tape3[2..k] AND t3h = num c in tape1[2..k] + 1 AND tape1[t3h] = BLANK
;;
;; F: tape 0 = (LM BLANK w) AND t0h = k >= 2
;;    tape 1 = (BLANK a* BLANK) AND num a = num a in tape1[2..k] AND t1h = num a in tape1[2..k] + 1 AND tape1[t1h] = BLANK 
;;    tape 2 = (BLANK b* BLANK) AND num b = num b in tape2[2..k] AND t2h = num b in tape1[2..k] + 1 AND tape1[t2h] = BLANK
;;    tape 3 = (BLANK c* BLANK) AND num c = num c in tape3[2..k] AND t3h = num c in tape1[2..k] + 1 AND tape1[t3h] = c
;;
;; G: tape 0 = (LM BLANK w)
;;    (drop t1 t1h) = a*
;;    (drop t2 t2h) = b*
;;    (drop t3 t3h) = c*
;;    (= |(drop t1 t1h)| |(drop t2 t2h)| |(drop t3 t3h)|)
;;
;; Y: num a in t0 = num b in t0 = num c in t0
;;
;; N: num a in t0 != num b in t0 != num c in t0



;; A tape is a (listof symbol)
;; A tape-config is a (list natnum tape)
;;  interpretation: the natnum is the position of the head on the given tape

;; State Documentation
;; S: tape 0 = (LM BLANK w) AND i = 1
;;    tape 1-3 = (BLANK) AND i = 0

;; (listof tape-config) --> Boolean
(define (S-INV tape-configs)
  (let* [(t0c (first tape-configs))
         (t1c (second tape-configs))
         (t2c (third tape-configs))
         (t3c (fourth tape-configs))
         (t0h (first t0c))
         (t0 (second t0c))
         (t1h (first t1c))
         (t1 (second t1c))
         (t2h (first t2c))
         (t2 (second t2c))
         (t3h (first t3c))
         (t3 (second t3c))]
    (and (= t0h 1)
         (= t1h 0)
         (= t2h 0)
         (= t3h 0)
         (eq? (list-ref t0 t0h) BLANK)
         (equal? `(,BLANK) t1)
         (equal? `(,BLANK) t2)
         (equal? `(,BLANK) t3))))

(check-equal? (S-INV (list (list 1 `(,LM ,BLANK a b c))
                           (list 0 `(,BLANK a))
                           (list 0 `(,BLANK b))
                           (list 0 `(,BLANK))))
              #f)
(check-equal? (S-INV (list (list 1 `(,LM ,BLANK a b c))
                           (list 0 `(,BLANK))
                           (list 0 `(,BLANK))
                           (list 0 `(,BLANK))))
              #t)

;; (listof tape-config) --> Boolean
(define (C-INV tape-configs)
  (let* [(t0c (first tape-configs))
         (t1c (second tape-configs))
         (t2c (third tape-configs))
         (t3c (fourth tape-configs))
         (t0h (first t0c))
         (t0 (second t0c))
         (t1h (first t1c))
         (t1 (second t1c))
         (t2h (first t2c))
         (t2 (second t2c))
         (t3h (first t3c))
         (t3 (second t3c))
         (readt0 (take (rest (rest t0)) (- t0h 2)))]
    (and (>= t0h 2)
         (eq? (list-ref t1 t1h) BLANK)
         (eq? (list-ref t2 t2h) BLANK)
         (eq? (list-ref t3 t3h) BLANK)
         (equal? (filter (λ (s) (eq? s 'a)) readt0)
                 (filter (λ (s) (eq? s 'a)) t1))
         (equal? (filter (λ (s) (eq? s 'b)) readt0)
                 (filter (λ (s) (eq? s 'b)) t2))
         (equal? (filter (λ (s) (eq? s 'c)) readt0)
                 (filter (λ (s) (eq? s 'c)) t3)))))

(check-equal? (C-INV (list (list 2 `(,LM ,BLANK b b b))
                           (list 0 `(,BLANK a a))
                           (list 0 `(,BLANK))
                           (list 0 `(,BLANK))))
              #f)
(check-equal? (C-INV (list (list 2 `(,LM ,BLANK b a c))
                           (list 0 `(,BLANK))
                           (list 0 `(,BLANK))
                           (list 0 `(,BLANK))))
              #t)
(check-equal? (C-INV (list (list 6 `(,LM ,BLANK b a b c a a b c))
                           (list 2 `(,BLANK a ,BLANK))
                           (list 3 `(,BLANK b b ,BLANK))
                           (list 2 `(,BLANK c ,BLANK))))
              #t)
         

;; PRE: (LM BLANK w) AND i = 1
(define EQABC (make-mttm '(S Y N C D E F G)
                         '(a b c)
                         'S
                         '(Y N)
                         (list (list (list 'S (list BLANK BLANK BLANK BLANK))
                                     (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                               (list (list 'C (list 'a BLANK BLANK BLANK))
                                     (list 'D (list 'a 'a BLANK BLANK)))
                               (list (list 'D (list 'a 'a BLANK BLANK))
                                     (list 'C (list RIGHT RIGHT BLANK BLANK)))
                               (list (list 'C (list 'b BLANK BLANK BLANK))
                                     (list 'E (list 'b BLANK 'b BLANK)))
                               (list (list 'E (list 'b BLANK 'b BLANK))
                                     (list 'C (list RIGHT BLANK RIGHT BLANK)))
                               (list (list 'C (list 'c BLANK BLANK BLANK))
                                     (list 'F (list 'c BLANK BLANK 'c)))
                               (list (list 'F (list 'c BLANK BLANK 'c))
                                     (list 'C (list RIGHT BLANK BLANK RIGHT)))
                               (list (list 'C (list BLANK BLANK BLANK BLANK))
                                     (list 'G (list BLANK LEFT LEFT LEFT)))
                               (list (list 'G (list BLANK BLANK BLANK BLANK))
                                     (list 'Y (list BLANK BLANK BLANK BLANK)))
                               (list (list 'G (list BLANK 'a 'b 'c))
                                     (list 'G (list BLANK LEFT LEFT LEFT)))
                               ;; too many of at least 1 letter
                               (list (list 'G (list BLANK BLANK 'b 'c))
                                     (list 'N (list BLANK BLANK 'b 'c)))
                               (list (list 'G (list BLANK 'a BLANK 'c))
                                     (list 'N (list BLANK 'a BLANK 'c)))
                               (list (list 'G (list BLANK 'a 'b BLANK))
                                     (list 'N (list BLANK 'a 'b BLANK)))
                               (list (list 'G (list BLANK BLANK BLANK 'c))
                                     (list 'N (list BLANK BLANK BLANK 'c)))
                               (list (list 'G (list BLANK BLANK 'b BLANK))
                                     (list 'N (list BLANK BLANK 'b BLANK)))
                               (list (list 'G (list BLANK 'a BLANK BLANK))
                                     (list 'N (list BLANK 'a BLANK BLANK)))
                               
                               

                               )
                         4
                         'Y))

(check-equal? (sm-apply EQABC `(,LM ,BLANK a a b b a c c) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK a a a) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK c c a b b) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply EQABC `(,LM ,BLANK a c c b a b) 1) 'accept)
(check-equal?
 (sm-apply EQABC `(,LM ,BLANK c c c a b b a a c b a b b c a) 1)
 'accept)