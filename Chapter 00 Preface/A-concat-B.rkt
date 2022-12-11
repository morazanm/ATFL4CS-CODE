#lang fsm

(define AB (concat-regexp (singleton-regexp "a")
                          (singleton-regexp "b")))

(check-equal? (printable-regexp AB) "ab")