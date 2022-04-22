#lang racket

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          guess
          (try next-guess))))
  try)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (let ((tolerance 0.00001))
      (< (abs (- v1 v2))
         tolerance)))
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  (define (good-enough? guess new-guess)
    (let ((tolerance 0.00001))
      (< (abs (- (square guess) x));don't use new-guess as fixed-point
         tolerance)))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
;;;;;;;;;;;;;;;;;
(sqrt 2)                ; 1.4142156862745097
(fixed-point cos 1.0)   ; 0.7390893414033928