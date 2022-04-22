#lang sicp

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess step)
    (display "step ")
    (display step)
    (display ": guess = ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ step 1)))))
  (try first-guess 1))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)

(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)