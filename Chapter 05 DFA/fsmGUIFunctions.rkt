#lang fsm

;; Created by fsm-GUI on: 8/2022 at 10:40am
;; This machine passed all tests.
(define N72 (make-dfa (quote (S F ds)) (quote (a b)) (quote S) (quote (F)) (quote ((F c D) (ds b ds) (ds a ds) (F b F) (F a ds) (S b ds) (S a F)))))
 
;; Created by fsm-GUI on: 12/2022 at 11:29am
;; This machine passed all tests.
(define W62 (make-dfa (quote (S F ds)) (quote (a b)) (quote S) (quote (F)) (quote ((ds b ds) (ds a ds) (F b F) (F a ds) (S b ds) (S a F)))))
