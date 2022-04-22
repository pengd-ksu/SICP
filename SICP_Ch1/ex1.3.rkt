#lang sicp

; 1.3takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (max2of3 x1 x2 x3)
  (cond ((and (>= x2 x1) (>= x3 x1)) (sum-of-squares x2 x3))
        ((and (>= x1 x2) (>= x3 x2)) (sum-of-squares x1 x3))
        ((and (>= x1 x3) (>= x2 x3)) (sum-of-squares x1 x2))))