#lang fsm

;; Syntactic Categories
;; S: generated words in a^nb^nc^n
;; A,I: A promise to generate an a in the context AI
;; B,H: A promise to generate an b in the context BH
;; C,G: A promise to generate an c in the context CG

(define anbncn-csg
  (make-csg '(S A B C G H I) 
            '(a b c) 
            `((S ,ARROW ABCS) 
              (S ,ARROW G)
              (BA ,ARROW AB) 
              (CA ,ARROW AC) 
              (CB ,ARROW BC)
              (CG ,ARROW Gc) 
              (G  ,ARROW H) 
              (BH ,ARROW Hb) 
              (H ,ARROW I)
              (AI ,ARROW Ia) 
              (I ,ARROW ,EMP)) 
            'S))

(check-equal? (grammar-derive anbncn-csg '())
              '(S -> G -> H -> I -> ε))
(check-equal? (grammar-derive anbncn-csg '(a a b b c c))
              '(S
                -> ABCS
                -> ABCABCS
                -> ABACBCS
                -> ABABCCS
                -> AABBCCS
                -> AABBCCG
                -> AABBCGc
                -> AABBGcc
                -> AABBHcc
                -> AABHbcc
                -> AAHbbcc
                -> AAIbbcc
                -> AIabbcc
                -> Iaabbcc
                -> aabbcc))