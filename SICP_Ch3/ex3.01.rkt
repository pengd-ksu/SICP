#lang sicp

(define (make-accumulator acc)
  (lambda (input)
    (set! acc (+ acc input))
    acc))

(define A (make-accumulator 5))

(A 10)

(A 10)