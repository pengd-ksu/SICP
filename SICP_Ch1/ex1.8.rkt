#lang sicp

; 1.1.7 Newton's method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (square x)
  (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (inc n)
  (+ n 1))
; Exercise 1.8
(define (cubrt x)  ; x is bounded to cubrt, so can be used for inside fc
  (define (cube x) ; defined inside, only visible inside cubrt
    (* x x x))
  (define (square x)
    (* x x))
  (define (close-enough? guess x)
    (and (< (/ (cube guess) x) 1.001)
         (> (/ (cube guess) x) 0.999)))
  (define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (cub-iter guess x)
    (if (close-enough? guess x)
        guess
        (cub-iter (improve guess x)
                  x)))
  (cub-iter 1.0 x))