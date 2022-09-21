#lang racket

(require fsm rackunit)

;; string --> (listof (o or c)) throws error
;; Purpose: Converts given string of parens to a list of parens
(define (parensstr->los s)
  (map (λ (p)
         (cond [(eq? p #\() 'o]
               [(eq? p #\)) 'c]
               [else (error (format "parensstr->los: non-parens symbol in: ~s" s))]))
       (string->list s)))

;; Tests for parensstr->los
(check-equal? (parensstr->los "") '())
(check-equal? (parensstr->los "(())()") '(o o c c o c))

;; Syntactic categories
;;   S = words with balanced parenthesis

;; L = {w | w has balanced parenthesis}, where o = ( and c = )
(define BP (make-cfg '(S)
                     '(o c)
                     `((S ,ARROW ,EMP)
                       (S ,ARROW SS)
                       (S ,ARROW oSc))
                     'S))

;; Tests for BP
(check-equal? (grammar-derive BP (parensstr->los ""))
              "The word () is too short to test.")
(check-equal? (grammar-derive BP (parensstr->los "))(("))
              "(c c o o) is not in L(G).")
(check-equal? (grammar-derive BP (parensstr->los "()("))
              "(o c o) is not in L(G).")
(check-equal? (grammar-derive BP (parensstr->los "()"))
              '(S -> oSc -> oc))
(check-equal? (grammar-derive BP (parensstr->los "(())()"))
              '(S -> SS -> oScS -> ooSccS -> ooccS -> ooccoSc -> ooccoc))
