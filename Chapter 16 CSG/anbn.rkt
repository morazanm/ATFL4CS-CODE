#lang fsm

(define anbn (make-csg '(S A B)
                       '(a b)
                       `((S ,ARROW AAaAAB)
                         (AAaAAA ,ARROW aSb)
                         (AAaAAA ,ARROW ,EMP)
                         (B ,ARROW A))
                       'S))

(check-equal? (grammar-derive anbn '()) '(S -> AAaAAB -> AAaAAA -> ε))
(check-equal? (grammar-derive anbn '(a a a b b b))
              '(S
                ->
                AAaAAB
                ->
                AAaAAA
                ->
                aSb
                ->
                aAAaAABb
                ->
                aAAaAAAb
                ->
                aaSbb
                ->
                aaAAaAABbb
                ->
                aaAAaAAAbb
                ->
                aaaSbbb
                ->
                aaaAAaAABbbb
                ->
                aaaAAaAAAbbb
                ->
                aaabbb))
