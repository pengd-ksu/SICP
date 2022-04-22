#lang racket

;Exercise 1.15
(define (cube x)
  (* x x x))
(define (p x)
  (display "call p, arg = ")
  (displayln x)
  (- (* 3 x)
     (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
; p was called five times, in a reversed order
; p is called when the angle over 3 is above 0.1; so the runtime is logarithmic
; of O(log3(n)). The space is also logarithmic as well, because this is a linear
; recursion in which each computation is a single recursive call