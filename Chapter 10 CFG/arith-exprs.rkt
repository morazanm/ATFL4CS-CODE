#lang fsm

(define AE (make-cfg '(S I)
                     '(p t x y z)
                     `((S ,ARROW SpS)
                       (S ,ARROW StS)
                       (S ,ARROW I)
                       (I ,ARROW x)
                       (I ,ARROW y)
                       (I ,ARROW z))
                     'S))

;; Tests for AE
(check-equal? (grammar-derive AE '(x p x))
              '(S -> SpS -> IpS -> xpS -> xpI -> xpx))
(check-equal? (grammar-derive AE '(x t z))
              '(S -> StS -> ItS -> xtS -> xtI -> xtz))
(check-equal? (grammar-derive AE '(x t z p y))
              '(S -> SpS -> StSpS -> ItSpS -> xtSpS -> xtIpS -> xtzpS -> xtzpI -> xtzpy))

(define AE2 (make-cfg '(S I T)
                      '(p t x y z)
                      `((S ,ARROW SpS)
                        (S ,ARROW T)
                        (S ,ARROW I)
                        (T ,ARROW TtT)
                        (T ,ARROW I)
                        (I ,ARROW x)
                        (I ,ARROW y)
                        (I ,ARROW z))
                      'S))

;; Tests for AE2
(check-equal? (grammar-derive AE2 '(x p x))
              '(S -> SpS -> IpS -> xpS -> xpI -> xpx))
(check-equal? (grammar-derive AE2 '(x t z))
              '(S -> T -> TtT -> ItT -> xtT -> xtI -> xtz))
(check-equal? (grammar-derive AE2 '(x t z p y))
              '(S -> SpS -> TpS -> TtTpS -> ItTpS -> xtTpS -> xtIpS -> xtzpS -> xtzpI -> xtzpy))
(check-equal? (grammar-derive AE2 '(x p z t z p y))
              '(S -> SpS -> SpSpS -> IpSpS -> xpSpS -> xpTpS -> xpTtTpS
                -> xpItTpS -> xpztTpS -> xpztIpS -> xpztzpS -> xpztzpI
                -> xpztzpy))
