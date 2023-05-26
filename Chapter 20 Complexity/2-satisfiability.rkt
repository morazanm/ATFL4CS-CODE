#lang fsm

;; isingleton --> symbol
;;            --> (not symbol)

;; i2disjunction is (or isingleton isingleton)

;; iclause --> isingleton
;;         --> i2disjunction

;; iformula --> (and (listof iclause))

;; An singleton is either
;;  1. A structure (make-var symbol)
;;  2. A structure (make-notvar symbol)

(struct var (symb) #:transparent)
(struct notvar (symb) #:transparent)

;; A clause is either
;;  1. singleton 
;;  2. (make-disjunction singleton singleton)
(struct 2disjunction (s1 s2) #:transparent)

;; A formula is a (listof clauses)
;; Interpretation: A conjunction


;; iformula --> formula
;; Purpose: Parse the given iformula
(define (parse-iformula an-if)
  
  ;; icl --> clause
  ;; Purpose: Parse the given icl
  (define (parse-iclause an-icl)
    ;; isingleton --> singleton
    ;; Purpose: Parse the given isingleton
    (define (parse-isingleton an-is)
      (if (symbol? an-is)
          (var an-is)
          (notvar (second an-is))))

    ;; idisjunction --> disjunction
    ;; Parse: Parse the given idisjunction
    (define (parse-i2disjunction an-id)
      (2disjunction (parse-isingleton (first an-id))
                    (parse-isingleton (second an-id))))

    ;; iclause --> Boolean
    ;; Purpose: Determine if the given iclause is an isingleton
    (define (isingleton? an-icl)
      (or (symbol? an-icl)
          (eq? (first an-icl) 'not)))
    
    (if (isingleton? an-icl)
        (parse-isingleton an-icl)
        (parse-i2disjunction (rest an-icl))))

  
  (map parse-iclause (rest an-if)))

(check-equal? (parse-iformula '(and x1 x2))
              (list (var 'x1) (var 'x2)))
(check-equal? (parse-iformula '(and (not i) j))
              (list (notvar 'i) (var 'j)))
(check-equal? (parse-iformula
               '(and (or a b) (or (not x) b)))
              (list
               (2disjunction (var 'a) (var 'b))
               (2disjunction (notvar 'x) (var 'b))))
(check-equal?
 (parse-iformula
  '(and (or (not n) m) r (or s (not t)) (not y)))
 (list
  (2disjunction (notvar 'n) (var 'm))
  (var 'r)
  (2disjunction (var 's) (notvar 't))
  (notvar 'y)))


;; formula --> Boolean
;; Purpose: Determine if the given formula is satisfiable
(define (2-satisfiability a-formula)
  
  ;; formula --> Boolean
  ;; Purpose: Determine if given formula has x and not x
  ;; Assumption: Given formula does not contain disjunctions
  (define (has-no-solution? a-formula)
    (cond [(empty? a-formula) #f]
          [(member (complement-singleton (first a-formula)) (rest a-formula)) #t]
          [else (has-no-solution? (rest a-formula))]))
  
  ;; singleton --> singleton
  ;; Purpose: Return the complement of the given singleton
  (define (complement-singleton a-singleton)
    (if (var? a-singleton)
        (notvar (var-symb a-singleton))
        (var (notvar-symb a-singleton))))

  ;; formula singleton --> formula
  ;; Purpose: Simplify the given formula by removing the given singleton
  ;; Assumption: The given formula does not have the complement of the given singleton as a clause
  (define (simplify-formula a-formula a-singleton)
    (define (simplify a-formula)
      (cond [(empty? a-formula) '()]
            [(equal? (first a-formula) a-singleton)
             (simplify-formula (rest a-formula) a-singleton)]
            [(not (2disjunction? (first a-formula)))
             (cons (first a-formula)
                   (simplify-formula (rest a-formula) a-singleton))]
            [(or (equal? (2disjunction-s1 (first a-formula)) a-singleton)
                 (equal? (2disjunction-s2 (first a-formula)) a-singleton))
             (simplify-formula (rest a-formula) a-singleton)]
            [(equal? (2disjunction-s1 (first a-formula))
                     (complement-singleton a-singleton))
             (cons (2disjunction-s2 (first a-formula))
                   (simplify-formula (rest a-formula) a-singleton))]
            [(equal? (2disjunction-s2 (first a-formula))
                     (complement-singleton a-singleton))
             (cons (2disjunction-s1 (first a-formula))
                   (simplify-formula (rest a-formula) a-singleton))]
            [else (cons (first a-formula)
                        (simplify-formula (rest a-formula) a-singleton))]))
    (remove-duplicates (simplify a-formula)))
      

  ;; formula (listof (symbol Boolean) --> (listof (list symbol Boolean))
  ;; Purpose: Find a variable assignment that satisfies the given Boolean formula
  ;; Accumulator invariant: acc = (listof (variable Boolean)) with current variable assignments
  (define (solve a-formula acc)
    ;(displayln (format "formula: ~s\nacc: ~s\n" a-formula acc))
    ;(read)
    (cond [(empty? a-formula) acc]
          [(has-no-solution? (filter (λ (c) (not (2disjunction? c)))
                                     a-formula))
           '()]
          [(ormap (λ (c) (or (var? c) (notvar? c))) a-formula)
           (let [(form-vars (filter var? a-formula))
                 (form-notvars (filter notvar? a-formula))]
             (solve (simplify-formula a-formula
                                       (if (null? form-notvars)
                                           (first form-vars)
                                           (first form-notvars)))
                    (if (null? form-notvars)
                        (cons (list (var-symb (first form-vars)) #t) acc)
                        (cons (list (notvar-symb (first form-notvars)) #f) acc))))]
          [else ;; a-formula only has 2disjunctions
           (let* [(fsingleton (2disjunction-s1 (first a-formula)))
                  (fvar (if (var? fsingleton)
                            (var-symb fsingleton)
                            (notvar-symb fsingleton)))
                  (not-fsingleton (complement-singleton fsingleton))
                  (sol1 (solve (simplify-formula a-formula fsingleton)
                               (cons (list fvar (var? fsingleton)) acc)))]
             (if (not (null? sol1))
                 sol1
                 (solve (simplify-formula a-formula not-fsingleton)
                        (cons (list fvar (notvar? fsingleton)) acc))))]))
  (if (not (null? (solve a-formula '())))
      'accept
      'reject))


(define F0 (parse-iformula '(and)))
(define F1 (parse-iformula '(and x (or (not y) (not x)) (or (not z) y) y)))

(define F2 (parse-iformula '(and (or x1 x2) x1 (or (not x1) (not x2)) (or (not x1) x2))))

(define F3 (parse-iformula '(and (or a b) a (or (not a) (not c)) (not c))))

(define F4 (parse-iformula '(and (or x1 x2)
                                 (or (not x1) (not x2))
                                 (or x2 x3)
                                 (or (not x1) (not x3)))))
(define F5 (parse-iformula '(and (or x1 (not x2))
                                 (or (not x1) (not x4))
                                 (or x2 (not x3))
                                 (or x1 x4)
                                 (or x3 x4))))


(check-equal? (2-satisfiability F0) 'reject)
(check-equal? (2-satisfiability F1) 'reject)
(check-equal? (2-satisfiability F2) 'reject)
(check-equal? (2-satisfiability F3) 'accept)
(check-equal? (2-satisfiability F4) 'accept)

