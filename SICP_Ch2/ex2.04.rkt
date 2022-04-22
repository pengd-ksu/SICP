#lang sicp

;; Exercise 2.4
(define (my-cons-2 x y)
  (lambda (m) (m x y)))

(define (my-car-2 z)
  (z (lambda (p q) p)))

(define (my-cdr-2 z)
  (z (lambda (p q) q)))

(define pair (my-cons-2 3 5))
(my-car-2 pair)
(my-cdr-2 pair)

; unrolling car
; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x

; unrolling cdr
; (cdr (cons x y))
; (cdr (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) q))
; ((lambda (p q) q) x y)
; y