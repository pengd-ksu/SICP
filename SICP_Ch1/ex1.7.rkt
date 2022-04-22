#lang sicp

; 1.1.7 Newton's method
(define (sqrt-iter guess x)
  (if (new-good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (/ (- (square guess)) x))0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))
;(sqrt (* 0.02 0.02)) -> 0.0354008825558513; for little number it's not accurate enough
;(sqrt 100000000000000000.0), will not stop. As stated by the author, in real computers,
;arithmetic operations are almost always performed with limited precision. Therefore, when
;the number is big, there is not enough space(?) for fraction. The result will not improve,
;so it runs into an endless loop
(define (new-good-enough? guess new-guess)
  (< (abs (/ (- guess new-guess) guess))
     0.001))
;(sqrt (* 0.03 0.03)) -> 0.03002766742182557
;(sqrt 100000000000000000.0) -> 316228564.9222876
; Both improves.