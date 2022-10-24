#lang racket

(require fsm rackunit)

;A password is a string that:
; Has length >= 10
; Includes at least one uppercase letter
; Includes at least one lowercase letter
; Includes at least one special character: $, &, !, and *

(define lowers '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define uppers '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(define spcls  '($ & ! *))

(define lc (map (λ (lcl) (singleton-regexp (symbol->string lcl)))
                lowers))
(define uc (map (λ (ucl) (singleton-regexp (symbol->string ucl)))
                uppers))
(define spc (map (λ (sc)
                   (singleton-regexp (symbol->string sc)))
                 spcls))

(define MAX-KLEENESTAR-REPS 5)

;; (listof regexp) --> union-regexp
;; Purpose: Create a union-regexp using the given list of regular expressions
(define (create-union-regexp L)
  (cond [(< (length L) 2) (error "create-union-regexp called with empty")]
        [(empty? (rest (rest L))) (union-regexp (first L) (second L))]
        [else (union-regexp (first L) (create-union-regexp (rest L)))]))

;; Tests
(check-equal? (create-union-regexp (list (first lc) (first uc)))
              (union-regexp (singleton-regexp "a") (singleton-regexp "A")))
(check-equal? (create-union-regexp (list (first lc) (fourth uc) (third spc)))
              (union-regexp
               (singleton-regexp "a")
               (union-regexp (singleton-regexp "D") (singleton-regexp "!"))))

(define LOWER (create-union-regexp lc))
(define UPPER (create-union-regexp uc))
(define SPCHS (create-union-regexp spc))
(define ARBTRY (kleenestar-regexp
                (union-regexp LOWER
                              (union-regexp UPPER SPCHS))))
(define LUS (concat-regexp
             ARBTRY
             (concat-regexp
              LOWER
              (concat-regexp
               UPPER
               (concat-regexp ARBTRY (concat-regexp SPCHS ARBTRY))))))

(define LSU (concat-regexp
             ARBTRY
             (concat-regexp
              LOWER
              (concat-regexp
               SPCHS
               (concat-regexp ARBTRY (concat-regexp UPPER ARBTRY))))))

(define SLU (concat-regexp
             ARBTRY
             (concat-regexp
              SPCHS
              (concat-regexp
               LOWER
               (concat-regexp ARBTRY (concat-regexp UPPER ARBTRY))))))

(define SUL (concat-regexp
             ARBTRY
             (concat-regexp
              SPCHS
              (concat-regexp
               UPPER
               (concat-regexp ARBTRY (concat-regexp LOWER ARBTRY))))))

(define USL (concat-regexp
             ARBTRY
             (concat-regexp
              UPPER
              (concat-regexp
               SPCHS
               (concat-regexp ARBTRY (concat-regexp LOWER ARBTRY))))))

(define ULS (concat-regexp
             ARBTRY
             (concat-regexp
              UPPER
              (concat-regexp
               LOWER
               (concat-regexp ARBTRY (concat-regexp SPCHS ARBTRY))))))

(define PASSWD (union-regexp
                LUS
                (union-regexp
                 LSU
                 (union-regexp
                  SLU
                  (union-regexp SUL
                                (union-regexp USL ULS))))))

;; regexp --> word
;; Purpose: Generate a random word in the language of the
;;          given regexp with a maximum length of 20
(define (gen-regexp-word rexp)

  ;; union-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps of the given union-regexp
  (define (extract-union-regexps urexp)
    (let [(r1 (union-regexp-r1 urexp))
          (r2 (union-regexp-r2 urexp))]
      (if (not (union-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-union-regexps r2)))))

  ;; concat-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps of the given concat-regexp
  (define (extract-concat-regexps crexp)
    (let [(r1 (concat-regexp-r1 crexp))
          (r2 (concat-regexp-r2 crexp))]
      (if (not (concat-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-concat-regexps r2)))))
  
  (cond [(empty-regexp? rexp) EMP]
        [(singleton-regexp? rexp)
         (let* [(element (singleton-regexp-a rexp))]
           (if (not (string<=? "0" element "9"))
               (list (string->symbol element))
               (list (string->number element))))]
        [(kleenestar-regexp? rexp)
         (let* [(reps (random MAX-KLEENESTAR-REPS))
                (element-list (flatten
                               (build-list
                                reps
                                (λ (i) (gen-regexp-word (kleenestar-regexp-r1 rexp))))))]
           (if (empty? element-list) EMP element-list))]
        [(union-regexp? rexp)
         (let* [(uregexps (extract-union-regexps rexp))
                (element (list-ref uregexps (random (length uregexps))))]
           (gen-regexp-word element))]
        [else (let [(cregexps (extract-concat-regexps rexp))]
                (filter (λ (w) (not (eq? w EMP)))
                        (flatten (map gen-regexp-word cregexps))))]))

;; word --> string
;; Purpose: Convert the given password to a string
(define (passwd->string passwd)
  (list->string
   (map (λ (s)
          (first (string->list (symbol->string s))))
        passwd)))

;;Tests
(check-equal? (passwd->string '(a j h B ! ! y y t c))
              "ajhB!!yytc")
(check-equal? (passwd->string '($ u t q x ! J i n * K C))
              "$utqx!Jin*KC")

;;  --> string
;; Purpose: Generate a valid password
(define (generate-password)
  (let [(new-passwd (passwd->string (gen-regexp-word PASSWD)))]
    (if (>= (string-length new-passwd) 10)
        new-passwd
        (generate-password))))

;;Tests

;; string --> (listof symbol)
;; Purpose: Convert the given string to a list of symbols
(define (str->los str)
  (map (λ (c) (string->symbol (string c))) (string->list str)))

;; Tests
(check-equal? (str->los "") '())
(check-equal? (str->los "a!Cop") '(a ! C o p))

;; string --> Boolean
;; Purpose: Test if the given string is a valid password
(define (is-passwd? p)
  (let [(los (str->los p))]
    (and (>= (length los) 10)
         (ormap (λ (c) (member c los)) lowers)
         (ormap (λ (c) (member c los)) uppers)
         (ormap (λ (c) (member c los)) spcls))))


(check-pred is-passwd? (generate-password))
(check-pred is-passwd? (generate-password))
(check-pred is-passwd? (generate-password))
(check-pred is-passwd? (generate-password))
(check-pred is-passwd? (generate-password))




