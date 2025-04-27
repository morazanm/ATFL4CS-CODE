#lang fsm

#|
Syntactic Categories
 S: generates words in a^nb^nc^n
 A,I: A promise to generate an a in the context AI
 B,H: A promise to generate an b in the context BH
 C,G: A promise to generate an c in the context CG
Invariant:
 •|A and a|=|B and b|=|C and c|
 • The yield contains at most one of the following
   nonterminals: S, G, H, I
 • S∈yield  ⇒ yield ends with S
 • G∈yield  ⇒ yield ends with Gc*
 • H∈yield  ⇒ yield ends with Hb*c+
 • I∈yield  ⇒ yield ends with Ia*b+c+
 • yield∈Σ* ⇒ yield∈L

|#

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

(check-derive? anbncn-csg '() '(a b c) '(a a b b c c)
               '(a a a b b b c c c))

(define (S-INV yield) (eq? (last yield) 'S))
(define (G-INV yield) ;; Only c’s right of G
  (= (count (lambda (x) (not (eq? 'c x)))
            (rest (dropf yield (lambda (x) (not (eq? 'G x))))))
     0))
(define (H-INV yield) ;; Only b’s and c’s right of H
  (let* ([bs-and-cs (rest (dropf yield
                                 (lambda (x) (not (eq? 'H x)))))]
         [cs (dropf bs-and-cs (lambda (x) (eq? x 'b)))])
    (andmap (lambda (x) (eq? x 'c)) cs)))

(define (I-INV yield)
  (let* ([as-bs-cs (rest (dropf yield (lambda (x) (not (eq? 'I x)))))]
         [bs-and-cs (dropf as-bs-cs (lambda (x) (eq? 'a x)))]
         [cs (dropf bs-and-cs (lambda (x) (eq? x 'b)))])
    (andmap (lambda (x) (eq? 'c x)) cs)))

(define (in-lang? yield)
  (let* ([as (takef yield (lambda (x) (eq? 'a x)))]
         [bs-and-cs (dropf yield (lambda (x) (eq? 'a x)))]
         [bs (takef bs-and-cs (lambda (x) (eq? 'b x)))]
         [cs (takef (dropf bs-and-cs (lambda (x) (eq? x 'b))) (lambda (x) (eq? 'c x)))])
    (= (length as) (length bs) (length cs))))

(define (equal-num-abc? word)
  (= (+ (count (lambda (x) (eq? 'A x)) word)
        (count (lambda (x) (eq? 'a x)) word))
     (+ (count (lambda (x) (eq? 'B x)) word)
        (count (lambda (x) (eq? 'b x)) word))
     (+ (count (lambda (x) (eq? 'C x)) word)
        (count (lambda (x) (eq? 'c x)) word))))

(define (no-nt? yield)
  (andmap (lambda (x) (member x (grammar-sigma anbncn-csg))) yield))

;; (listof (N ∪ Σ) → Boolean
;; Purpose: Determine if loop invariant holds for the given yield
(define (anbncn-csg-inv yield)
  (and (equal-num-abc? yield) ;; equal number A/a,B/b,C/c
       (implies (member 'S yield) (S-INV yield))
       (implies (member 'G yield) (G-INV yield))
       (implies (member 'H yield) (H-INV yield))
       (implies (member 'I yield) (I-INV yield))
       (implies (no-nt? yield) (in-lang? yield))))

;; (grammar-viz anbncn-csg '(a a a b b b c c c) anbncn-csg-inv)

#|
Proof by induction on n=number of derivation steps

Base case:
When derivation starts, yield=S.
S-INV holds a S is the rightmost element of the yield) ∧
the number of As, as, Bs, bs, Cs, and cs are all 0 ∧
other implications are true (i.e., false ⇒ X is true)

Inductive Step
Assume: INV holds for n=k
Show: INV holds for n=k+1

S → EMP
By inductive hypothesis INV holds
After applying this production rule:
the number of As, as, Bs, bs, Cs, and cs are all 0 ∧
(implies (no-nt? yield) (in-lang? yield)) holds ∧
other implications are true (i.e., false ⇒ X is true)

S → ABCS
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
S ends the yield ∧
other implications are true (i.e., false ⇒ X is true)

BA → AB, CB → BC, CA → AC
By inductive hypothesis INV holds
After applying any of these production rules:
INV holds because only the ordering of A, B, & C change

S → G
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of G is c ∧
other implications are true (i.e., false ⇒ X is true)

CG → Gc
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of G is c+ ∧
other implications are true (i.e., false ⇒ X is true)

BG → BH
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of H is b∗c+ ∧
other implications are true (i.e., false ⇒ X is true)

BH → Hb
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of H is b+c+ ∧
other implications are true (i.e., false ⇒ X is true)

AH → AI
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of I is a∗b+c+ ∧
other implications are true (i.e., false ⇒ X is true)

AI → Ia
By inductive hypothesis INV holds
After applying this production rule:
|A & a|=|B & b|=|C & c| ∧
Everything to the right of I is a+b+c+ ∧
other implications are true (i.e., false ⇒ X is true)

I → EMP
After applying this production rule:
|a|=|b|=|c| ∧
yield is a+b+c+ ∧
other implications are true (i.e., false ⇒ X is true)


Proof that L=L(anbncn)
w∈L ⇔ w∈L(anbncn)
(⇒) Assume w∈L
This means w=anbncn. Given that INV always holds, there
is a derivation such that S generates n ABC, rearranges
ABCs to AnBnCn, and generates w.

(⇐)) Assume w∈L(anbncn)
This means anbncn generated w. Since INV always holds
w=anbncn. Thus, w∈L.

w∈/L ⇔ w∈/L(anbncn)
Contraposition

|#