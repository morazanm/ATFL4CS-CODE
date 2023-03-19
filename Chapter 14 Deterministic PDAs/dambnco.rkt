#lang fsm

;; L = {a^mb^nc^p | m != n and m,n,p > 0}
;; State Documentation
;;  S: ci = stack = '(), starting state
;;  A: ci = a^+ and stack = a^+y
;;  B: ci = a^ib^j and stack = a^(i-j)y and 0 < i <= j
;;  C: ci = a^ib^jc^k and stack = a^(i-j-k)y and i > j and i,j,k > 0
;;  D: ci = a^ib^j and stack = y and j > i and i,j > 0
;;  E: ci = a^ib^jc^+ and stack = y, j != i and i,j > 0
;;  G: ci = a^ib^jc^+z and stack = a*y and i != j and i,j > 0
;;  F: ci = a^ib^jc^+z and stack = '() and i != j, and i,j > 0, final state

(define ambnco (make-ndpda '(S A B C D E F G)
                           '(a b c z)
                           '(a y)
                           'S
                           '(F)
                           `(((S a ,EMP) (A (a y))) ;; at least 1 a
                             ((A a ,EMP) (A (a)))
                             ((A b (a)) (B ,EMP)) ;; at least 1 b
                             ((B b (a)) (B ,EMP))
                             ((B c (a)) (C ,EMP)) ;; more a's
                             ((C c (a)) (C ,EMP))
                             ((C z ,EMP) (G ,EMP))
                             ((C c (y)) (E (y))) ;; at least 1 c
                             ((B b (y)) (D (y))) ;; more b's
                             ((D b (y)) (D (y)))
                             ((D c (y)) (E (y))) ;; at least 1 c
                             ((E c (y)) (E (y)))
                             ((E z (y)) (F ,EMP))
                             ((G ,EMP (a)) (G ,EMP))
                             ((G ,EMP (y)) (F ,EMP)))))

(check-equal? (sm-apply ambnco '(z)) 'reject)
(check-equal? (sm-apply ambnco '(a b b c c)) 'reject)
(check-equal? (sm-apply ambnco '(a a b b c c z)) 'reject)
(check-equal? (sm-apply ambnco '(a b a z)) 'reject)
(check-equal? (sm-apply ambnco '(a b b z)) 'reject)
(check-equal? (sm-apply ambnco '(a a a b b c c z)) 'accept)
(check-equal? (sm-apply ambnco '(a a a b c z)) 'accept)
(check-equal? (sm-apply ambnco '(a a a b b b b c c c z)) 'accept)

;; word stack --> Boolean
(define (S-INV ci s)
  (and (eq? ci '()) (eq? s '())))

(check-equal? (S-INV '(a a) '(a)) #f)
(check-equal? (S-INV '() '(a)) #f)
(check-equal? (S-INV '(a) '()) #f)
(check-equal? (S-INV '() '()) #t)

;; word stack --> Boolean
(define (A-INV ci s)
  (and  (not (empty? ci))
        (andmap (λ (r) (eq? r 'a)) ci)
        (eq? (last s) 'y)
        (= (length (takef s (λ (r) (eq? r 'a))))
           (length ci))))

(check-equal? (A-INV '(a b) '(a)) #f)
(check-equal? (A-INV '(a) '(a a y)) #f)
(check-equal? (A-INV '(a a a) '(a a a y)) #t)
(check-equal? (A-INV '(a) '(a y)) #t)


;; word stack --> Boolean
(define (B-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))
         (s-as (takef s (λ (r) (eq? r 'a))))]
    (and (equal? ci (append ci-as ci-bs))
         (< 0 i)
         (< 0 j)
         (<= j i)
         (eq? (last s) 'y)
         (equal? s (append s-as '(y)))
         (= (length s-as) (- i j)))))

(check-equal? (B-INV '(b b) '(a)) #f)
(check-equal? (B-INV '(a a b b) '(a y)) #f)
(check-equal? (B-INV '(a a b b) '(y)) #t)
(check-equal? (B-INV '(a a a a b b) '(a a y)) #t)


;; word stack --> Boolean
(define (C-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))
         (ci-cs (takef (drop ci (+ (length ci-as)
                                   (length ci-bs)))
                       (λ (r) (eq? r 'c))))
         (k (length ci-cs))
         (s-as (takef s (λ (r) (eq? r 'a))))]
    (and (equal? ci (append ci-as ci-bs ci-cs))
         (eq? (last s) 'y)
         (equal? s (append s-as '(y)))
         (= (length s-as) (- i j k))
         (> i j)
         (> i 0)
         (> j 0))))

(check-equal? (C-INV '(a a b c z) '(a y)) #f)
(check-equal? (C-INV '(a a b c) '(a y)) #f)
(check-equal? (C-INV '(a a b) '(a y)) #t)
(check-equal? (C-INV '(a a a a b c) '(a a y)) #t)


;; word stack --> Boolean
(define (D-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))]
    (and (equal? ci (append ci-as ci-bs))
         (equal? s '(y))
         (> j i)
         (> i 0)
         (> j 0))))
    
(check-equal? (D-INV '(a a b c c z) '(y)) #f)
(check-equal? (D-INV '(a) '(a y)) #f)
(check-equal? (D-INV '(a b b) '(y)) #t)
(check-equal? (D-INV '(a a b b b) '(y)) #t)


;; word stack --> Boolean
(define (E-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))
         (ci-cs (takef (drop ci (+ (length ci-as)
                                   (length ci-bs)))
                       (λ (r) (eq? r 'c))))]
    (and (equal? ci (append ci-as ci-bs ci-cs))
         (>= (length ci-cs) 1)
         (equal? s '(y))
         (not (= j i))
         (> i 0)
         (> j 0))))

(check-equal? (E-INV '(a b) '(a y)) #f)
(check-equal? (E-INV '(a a b) '(a y)) #f)
(check-equal? (E-INV '(a b b c) '(y)) #t)
(check-equal? (E-INV '(a a a b c c) '(y)) #t)


;; word stack --> Boolean
(define (G-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))
         (ci-cs (takef (drop ci (+ (length ci-as)
                                   (length ci-bs)))
                       (λ (r) (eq? r 'c))))
         (s-as (takef s (λ (r) (eq? r 'a))))]
    (and (equal? ci (append ci-as ci-bs ci-cs '(z)))
         (>= (length ci-cs) 1)
         (equal? s (append s-as '(y)))
         (not (= j i))
         (> i 0)
         (> j 0))))
         
(check-equal? (G-INV '(a) '(a y)) #f)
(check-equal? (G-INV '(a b c z) '(y)) #f)
(check-equal? (G-INV '(a a a b c z) '(a y)) #t)
(check-equal? (G-INV '(a a a b c c z) '(a a y)) #t)


;; word stack --> Boolean
(define (F-INV ci s)
  (let* [(ci-as (takef ci (λ (r) (eq? r 'a))))
         (i (length ci-as))
         (ci-bs (takef (drop ci (length ci-as))
                       (λ (r) (eq? r 'b))))
         (j (length ci-bs))
         (ci-cs (takef (drop ci (+ (length ci-as)
                                   (length ci-bs)))
                       (λ (r) (eq? r 'c))))]
    (and (equal? ci (append ci-as ci-bs ci-cs '(z)))
         (>= (length ci-cs) 1)
         (empty? s)
         (not (= j i))
         (> i 0)
         (> j 0))))

(check-equal? (F-INV '(z) '()) #f)
(check-equal? (F-INV '(a a b b c cz) '()) #f)
(check-equal? (F-INV '(a a b z) '(y)) #f)
(check-equal? (F-INV '(a a b c z) '()) #t)
(check-equal? (F-INV '(a b b b c c z) '()) #t)


#;(sm-visualize ambnco
              (list 'S S-INV)
              (list 'A A-INV)
              (list 'B B-INV)
              (list 'C C-INV)
              (list 'D D-INV)
              (list 'E E-INV)
              (list 'F F-INV)
              (list 'G G-INV))