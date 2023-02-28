#lang fsm

;(define ADD-CSG (make-csg '(S I J K L M N)
;                          '(b i j)
;                          `((S ,ARROW Ib)
;                            (I ,ARROW iIi)
;                            (I ,ARROW bJb)
;                            (J ,ARROW jJ)
;                            (J ,ARROW jJj)
;                            (J ,ARROW K)
;                            (jK ,ARROW Kj)
;                            (bKj ,ARROW biL)
;                            (Lj ,ARROW jL)
;                            (Lb ,ARROW bL)
;                            (Li ,ARROW iL)
;                            (Lb ,ARROW Mib)
;                            (iM ,ARROW Mi)
;                            (ibM ,ARROW ib)
;                            (bM ,ARROW Mb)
;                            (jM ,ARROW Nj)
;                            (jN ,ARROW Nj)
;                            (iNj ,ARROW iiL))
;                          'S))

;; L = {AbBbAB | A,B in i*}
;; Syntactic Categories
;;  S: generates words in i^nbi^mbi^ni^m
;;  A: generates words in i^* and, for every i generated, a promise  
;;     to generate a matching i for the result
;;  I: generates an i for the result in the context IE
;;  E: generates zero i or one i for the result in the context IE
(define ADD-CSG2 (make-csg '(S A E I)
                           '(b i)
                           `((S ,ARROW AbAbE)
                             (A ,ARROW ,EMP)
                             (A ,ARROW iIA)
                             (Ii ,ARROW iI)
                             (Ib ,ARROW bI)
                             (IE ,ARROW Ei)
                             (E ,ARROW ,EMP))
                           'S))

;; Tests
(check-equal? (grammar-derive ADD-CSG2 '(b b))
              '(S -> AbAbE -> AbAb -> Abb -> bb))
(check-equal? (last (grammar-derive ADD-CSG2 '(b i b i)))
              'bibi)
(check-equal? (last (grammar-derive ADD-CSG2 '(i b b i)))
              'ibbi)
(check-equal? (last (grammar-derive ADD-CSG2 '(i i b i i b i i i i)))
              'iibiibiiii)
#;(check-equal? (last (grammar-derive ADD-CSG2 '(i i b i i i b i i i i i)))
              'iibiiibiiiii)