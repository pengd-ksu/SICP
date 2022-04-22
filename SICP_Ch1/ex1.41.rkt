#lang sicp

(define (inc n)
  (+ n 1))

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
; (double double) applies twice to the argument f, and f here (double) applies
; twice to its own argument. Therefore, (double double) applies f (double) twice,
; which is 4 times (2 * 2) in total
; (double (double double)) applies f (double double) twice, since f applies 4 times,
; it will apply 4 * 4 = 16 times.