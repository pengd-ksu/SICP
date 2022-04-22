#lang racket

(define (square x)
  (* x x))

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
; unrolling procedure: (f f) -> (f 2) -> (2 2), while 2 is not a procedure