#lang sicp

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (cube x)
  (* x x x))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; test case from:
; https://github.com/hjcapple/reading-sicp/blob/master/chapter_1/exercise_1_40.scm
;;;;;;;;;;;;;;;;;;;;;;;;;
(newtons-method (cubic 3 2 1) 1.0)  ; -2.3247179572447
(newtons-method (cubic 3 4 5) 1.0)  ; -2.2134116627622
(newtons-method (cubic 6 7 8) 1.0)  ; -4.9054740060655