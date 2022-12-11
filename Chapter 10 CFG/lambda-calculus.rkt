#lang fsm

(define IDS '(a b d e f g h i j k l m n p q r s t u v w x y z))

(define LC (make-cfg '(E I)
                     (append '(λ o c) IDS)
                     (append
                      `((E ,ARROW I)
                        (E ,ARROW oλoIcEc)
                        (E ,ARROW oEEc))
                      (map (λ (a) `(I ,ARROW ,a)) IDS))
                     'E))

;; Tests for LC
(check-equal? (grammar-derive LC '(a))
              "The word (a) is too short to test.")
(check-equal? (grammar-derive LC '(o λ o a c b c))
              '(E -> oλoIcEc -> oλoacEc -> oλoacIc -> oλoacbc))
(check-equal? (grammar-derive LC '(o a b c))
              '(E -> oEEc -> oIEc -> oaEc -> oaIc -> oabc))
(check-equal? (grammar-derive LC '(o o λ o a c a c o λ o b c b c c))
              '(E
                ->
                oEEc
                ->
                ooλoIcEcEc
                ->
                ooλoacEcEc
                ->
                ooλoacIcEc
                ->
                ooλoacacEc
                ->
                ooλoacacoλoIcEcc
                ->
                ooλoacacoλobcEcc
                ->
                ooλoacacoλobcIcc
                ->
                ooλoacacoλobcbcc))

(check-equal? (grammar-derive LC '(o o λ o z c o g z c c o h x c c))
              '(E
                -> oEEc
                -> ooλoIcEcEc
                -> ooλozcEcEc
                -> ooλozcoEEccEc
                -> ooλozcoIEccEc
                -> ooλozcogEccEc
                -> ooλozcogIccEc
                -> ooλozcogzccEc
                -> ooλozcogzccoEEcc
                -> ooλozcogzccoIEcc
                -> ooλozcogzccohEcc
                -> ooλozcogzccohIcc
                -> ooλozcogzccohxcc))

(define (lcalc->LC e)
  (if (symbol? e)
      e
      (map (λ (s) (cond [(eq? s '|(|) 'o]
                        [(eq? s '|)|) 'c]
                        [else s]))
           e)))

;; Tests for lcalc->LC
(check-equal? (lcalc->LC 'x) 'x)


(struct var-expr (id) #:transparent)
(struct lambda-expr (param body) #:transparent)
(struct app-expr (rator rand) #:transparent)

(define (get-param-lexp lexpr) (fourth lexpr))

(check-equal? (get-param-lexp '(o λ o b c o a b c c)) 'b)
(check-equal? (get-param-lexp '(o λ o a c a c)) 'a)

(define (get-body-lexp lexpr)
  (let [(body (drop-right (drop lexpr 5) 1))]
    (if (= (length body)1)
        (first body)
        body)))

(check-equal? (get-body-lexp '(o λ o b c o a b c c))
              '(o a b c))
(check-equal? (get-body-lexp '(o λ o a c a c)) 'a)

(define (get-rator-appexpr appe)
  (define (extract-expr e res sum)
    ;(define d (displayln (format "e = ~s\nres = ~s\nsum = ~s\n" e res sum)))
    (if (= sum 0)
        (reverse res)
        (extract-expr
         (rest e)
         (cons (first e) res)
         (cond [(eq? (first e) 'o) (add1 sum)]
               [(eq? (first e) 'c) (sub1 sum)]
               [else sum]))))
  (if (not (eq? 'o (second appe)))
      (second appe)
      (extract-expr (drop appe 2) '(o) 1)))

(check-equal? (get-rator-appexpr '(o a b c)) 'a)
(check-equal? (get-rator-appexpr '(o o λ o a c a c b c))
              '(o λ o a c a c))
(check-equal? (get-rator-appexpr '(o o λ o b c o d a c c o d b c c))
              '(o λ o b c o d a c c))

(define (get-rand-appexpr appe len-rator)
  (let [(e (drop-right (drop appe (add1 len-rator)) 1))]
    (if (= (length e) 1)
        (first e)
        e)))

(check-equal? (get-rand-appexpr '(o a b c) 1) 'b)
(check-equal? (get-rand-appexpr '(o o λ o a c a c b c) 7) 'b)
(check-equal? (get-rand-appexpr '(o o λ o b c o d a c c o d b c c) 10) '(o d b c))

;; word --> lc-parse-tree
;; Purpose: Parse the given lce
(define (parse-lce lce)
  (define (parse w)
    (cond [(symbol? w) (var-expr w)]
          [(equal? (take w 2) '(o λ))
           (let [(param (get-param-lexp w))
                 (body (get-body-lexp w))]
             (lambda-expr param (parse-lce body)))]
          [else (let* [(op (get-rator-appexpr w))
                       (arg (get-rand-appexpr w (if (symbol? op) 1 (length op))))]
                  (app-expr (parse-lce op) (parse-lce arg)))]))
  (if (symbol? lce)
      (var-expr lce)
      (parse lce)))

(check-equal? (parse-lce 'x) (var-expr 'x))
(check-equal? (parse-lce '(o λ o y c o f y c c))
              (lambda-expr 'y (app-expr (var-expr 'f) (var-expr 'y))))
(check-equal? (parse-lce '(o b a c))
              (app-expr (var-expr 'b) (var-expr 'a)))
(check-equal? (parse-lce '(o o λ o z c o g z c c o h x c c))
              (app-expr
               (lambda-expr 'z (app-expr (var-expr 'g) (var-expr 'z)))
               (app-expr (var-expr 'h) (var-expr 'x))))
