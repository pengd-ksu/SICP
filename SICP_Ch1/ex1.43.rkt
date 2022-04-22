#lang sicp

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1))
               f)))

((repeated square 2) 5)