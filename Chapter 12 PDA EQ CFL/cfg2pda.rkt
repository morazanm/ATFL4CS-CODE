#lang racket

(require fsm rackunit)

;; Sample cfg

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))

;; L = {w | w in (a b)* AND  w has more b than a}
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; cfg --> pda
;; Purpose: Transform the given cfg into a pda
(define (cfg2pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (append nts sigma)
                'S
                (list 'Q)
                (append
                 (list (list (list 'S EMP EMP) (list 'Q (list start))))
                 (map (λ (r)
                        (list (list 'Q EMP (list (first r)))
                              (list 'Q
                                    (if (eq? (third r) EMP)
                                        EMP
                                        (symbol->fsmlos (third r))))))
                      rules)
                 (map (λ (a) (list (list 'Q a (list a)) (list 'Q EMP)))
                      sigma)))))

;; Tests

(define a2nb2n-pda (cfg2pda a2nb2n))
(define numb>numa-pda (cfg2pda numb>numa))

(check-equal? (sm-apply a2nb2n-pda '(b b)) 'reject)
(check-equal? (sm-apply a2nb2n-pda '(a a b b a b)) 'reject)
(check-equal? (sm-apply a2nb2n-pda '()) 'accept)
(check-equal? (sm-apply a2nb2n-pda '(a a a b b b)) 'accept)

(check-equal? (sm-apply numb>numa-pda '(b b b)) 'accept)
(check-equal? (sm-apply numb>numa-pda '(b b a)) 'accept)
(check-equal? (sm-apply numb>numa-pda '(b b a b b)) 'accept)
;(check-equal? (sm-apply numb>numa-pda '(a b b a b)) 'accept) ;; ~3 mins

