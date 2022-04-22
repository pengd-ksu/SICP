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

(define (smooth f)
  (lambda (x)
    (let ((dx 0.00001))
      (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
         3.0))))

(define (smooth-n-fold f n)
  (repeated smooth n) f)

;;;;;;;;;;;;;;;;;;;;;;
((smooth square) 5)             ; 25.000000000066663
((smooth-n-fold square 10) 5)  ; 25.000000000666663