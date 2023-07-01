#lang fsm

;; L = ab* U ba*
#;(define aUb-ba*Uab* (make-ndfa '(S A B C)
                               '(a b)
                               'S
                               '(B C)
                               '((S a A)
                                 (S b A)
                                 (A a B)
                                 (A b C)
                                 (B b B)
                                 (C a C))))
(define aUb-ba*Uab* (make-ndfa '(S A B C D)
                               '(a b)
                               'S
                               '(C D)
                               `((S ,EMP A)
                                 (S ,EMP B)
                                 (A a C)
                                 (C b C)
                                 (B b D)
                                 (D a D)
                                 )))

;; L = b*
(define b* (make-ndfa `(,DEAD S A)
                      '(a b)
                      'S
                      '(A)
                      `((S ,EMP A)
                        (S a ,DEAD)
                        (A b A))))

;; L = {}
(define EMPTY (make-ndfa '(S)
                         '(a b)
                         'S
                         '()
                         '()))
                             
;; L = {w | w has an even number of a and an odd number of b}
(define EVEN-A-ODD-B (make-dfa '(S M N P)
                               '(a b)
                               'S
                               '(N)
                               '((S a P)
                                 (S b N)
                                 (M a N)
                                 (M b P)
                                 (N a M)
                                 (N b S)
                                 (P a S)
                                 (P b M))
                               'no-dead))

;; Data Definitions
;;
;; A node is a symbol
;;
;; An edge, (list node regexp node), has a beginning node, a
;; regular expression for its label, and destination node.

;; A directed graph, dgraph, is a (listof edge)



;; (listof ndfa-rule) --> dgraph
;; Purpose: Create a dgraph from the given ndfa
(define (make-dgraph lor)
  (map (λ (r)
         (if (eq? (second r) EMP)
             (list (first r) (empty-regexp) (third r))
             (list (first r)
                   (singleton-regexp (symbol->string (second r)))
                   (third r))))
       lor))

;; Tests for make-dgraph
(check-equal? (make-dgraph '()) '())
(check-equal?
 (make-dgraph `((S ,EMP A)
                (S a ,DEAD)
                (A b A)))
 (list
  (list 'S (empty-regexp) 'A)
  (list 'S (singleton-regexp "a") 'ds)
  (list 'A (singleton-regexp "b") 'A)))
(check-equal?
 (make-dgraph '((S a A)
                (S b A)
                (A a B)
                (A b C)
                (B b B)
                (C a C)))
 (list
  (list 'S (singleton-regexp "a") 'A)
  (list 'S (singleton-regexp "b") 'A)
  (list 'A (singleton-regexp "a") 'B)
  (list 'A (singleton-regexp "b") 'C)
  (list 'B (singleton-regexp "b") 'B)
  (list 'C (singleton-regexp "a") 'C)))


;; (listof edge) --> regexp
;; Purpose: Collapse the given edges into a regexp
(define (collapse-edges loe)
  (cond [(empty? loe) '()]
        [(empty? (rest loe)) (second (first loe))]
        [else (union-regexp (second (first loe))
                            (collapse-edges (rest loe)))]))

;; Tests for collapse-edges
(check-equal? (collapse-edges '())'())
(check-equal? (collapse-edges `((S ,(singleton-regexp "a") S)))
              (singleton-regexp "a"))
(check-equal? (collapse-edges `((A ,(singleton-regexp "a") A)
                                (A ,(singleton-regexp "b") A)
                                (A ,(empty-regexp) A)))
              (union-regexp (singleton-regexp "a")
                            (union-regexp (singleton-regexp "b")
                                          (empty-regexp))))

;; dgraph --> dgraph
;; Purpose: Collapse multiple edges between nodes
(define (remove-multiple-edges g)
  ;(define d (displayln (format "g is: ~s\n" g)))
  (if (empty? g)
      '()
      (let* [(curr-edge (first g))
             (from-state (first curr-edge))
             (to-state (third curr-edge))
             (to-collapse (filter (λ (e) (and (eq? (first e) from-state)
                                              (eq? (third e) to-state)))
                                  g))
             (remaining-g (filter (λ (e) (not (member e to-collapse))) g))]
        (cons (list from-state (collapse-edges to-collapse) to-state)
              (remove-multiple-edges remaining-g)))))

;; Tests for remove-multiple-edges
(check-equal? '()'())
(check-equal?
 (remove-multiple-edges `((S ,(singleton-regexp "a") A)
                          (S ,(singleton-regexp "b") A)
                          (A ,(singleton-regexp "a") A)))
 `((S
    ,(union-regexp (singleton-regexp "a") (singleton-regexp "b"))
    A)
   (A ,(singleton-regexp "a") A)))

;; node dgraph --> dgraph
;; Purpose: Rip out given state from given graph
(define (rip-out-node n g)
  ;(define d (displayln (format "removing: ~s\n from: ~s\n" s g)))
  (let* [(non (filter (λ (r) (and (not (eq? (third r) n))
                                          (not (eq? (first r) n))))
                              g))
         (into-n (filter (λ (r) (and (eq? (third r) n)
                                           (not (eq? (first r) n))))
                               g))
         (outof-n (filter (λ (r) (and (eq? (first r) n)
                                            (not (eq? (third r) n))))
                                g))
         (self-edges (filter (λ (r) (and (eq? (first r) n)
                                         (eq? (third r) n)))
                             g))
         ;(dd (displayln (format "into ~s: ~s\n" s into-s)))
         ;(ddd (displayln (format "outof ~s: ~s\n" s outof-s)))
         ;(dddd (displayln (format "self on ~s: ~s\n" s self-edges)))
         ]
    (remove-multiple-edges
     (append
      non
      (if (not (empty? self-edges))
          (let [(self-edge (first self-edges))]
            (append-map (λ (into-edge)
                          (map (λ (outof-edge) (list (first into-edge)
                                                     (concat-regexp (second into-edge)
                                                                    (concat-regexp (kleenestar-regexp (second self-edge))
                                                                                   (second outof-edge)))
                                                     (third outof-edge)))
                               outof-n))
                        into-n))
          (append-map (λ (into-edge)
                        (map (λ (outof-edge) (list (first into-edge)
                                                   (concat-regexp (second into-edge)
                                                                  (second outof-edge))
                                                   (third outof-edge)))
                             outof-n))
                      into-n))))))

;; Tests for rip-out-state
(check-equal? (rip-out-node 'A `((S ,(singleton-regexp "a") A)
                                 (A ,(singleton-regexp "b") B)))
              `((S
                 ,(concat-regexp (singleton-regexp "a")
                                 (singleton-regexp "b"))
                 B)))

(check-equal? (rip-out-node 'C `((S ,(singleton-regexp "a") A)
                                 (S ,(singleton-regexp "b") B)
                                 (A ,(singleton-regexp "a") C)
                                 (B ,(singleton-regexp "b") C)
                                 (C ,(singleton-regexp "a") D)
                                 (C ,(singleton-regexp "b") E)))
              `((S ,(singleton-regexp "a") A)
                (S ,(singleton-regexp "b") B)
                (A
                 ,(concat-regexp (singleton-regexp "a") (singleton-regexp "a"))
                 D)
                (A
                 ,(concat-regexp (singleton-regexp "a") (singleton-regexp "b"))
                 E)
                (B
                 ,(concat-regexp (singleton-regexp "b") (singleton-regexp "a"))
                 D)
                (B
                 ,(concat-regexp (singleton-regexp "b") (singleton-regexp "b"))
                 E)))
         

;; (listof node) dgraph --> dgraph
;; Purpose: Rip out the given nodes from the given graph
(define (rip-out-nodes lon g)
  (foldr (λ (s g) (rip-out-node s g)) g lon))

;; Tests for rip-out-states
(check-equal? (rip-out-nodes '() `((S ,(singleton-regexp "a") A)
                                   (A ,(singleton-regexp "b") B)))
              `((S ,(singleton-regexp "a") A)
                (A ,(singleton-regexp "b") B)))

(check-equal? (rip-out-nodes '(A B) `((S ,(singleton-regexp "a") A)
                                      (A ,(singleton-regexp "b") B)
                                      (B ,(singleton-regexp "b") C)))
              `((S
                 ,(concat-regexp (singleton-regexp "a")
                                 (concat-regexp
                                  (singleton-regexp "b")
                                  (singleton-regexp "b")))
                 C)))

              

;; ndfa --> regexp
;; Purpose: Create a regexp from the given ndfa
;; Assume: The transition diagram of the given machine is a connected directed graph
(define (ndfa2regexp m)
  (let* [(new-start (generate-symbol 'S (sm-states m)))
         (new-final (generate-symbol 'F (sm-states m)))
         (init-dgraph (make-dgraph
                       (cons (list new-start EMP (sm-start m))
                             (append (map (λ (f)
                                            (list f EMP new-final))
                                          (sm-finals m))
                                     (sm-rules m)))))
         (collapsed-dgraph (rip-out-nodes (sm-states m)
                                          (remove-multiple-edges init-dgraph)))]
    (if (empty? collapsed-dgraph)
        (null-regexp)
        (simplify-regexp (second (first collapsed-dgraph))))))

;; Tests for ndfa2regexp
(check-equal? (printable-regexp (ndfa2regexp EMPTY))
              "()")
(check-equal? (printable-regexp (ndfa2regexp b*))
              "b*")
(check-equal? (printable-regexp (ndfa2regexp aUb-ba*Uab*))
              "(b U a)(ab* U ba*)")
(check-equal?
 (printable-regexp (ndfa2regexp EVEN-A-ODD-B))
 "((ba U ab)(aa U bb)*(ab U ba) U (aa U bb))*((ba U ab)(aa U bb)*a U b)")



