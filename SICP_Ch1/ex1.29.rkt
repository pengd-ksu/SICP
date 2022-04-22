#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (let ((h (/ (- b a) n)))
      (* (factor k) h (f (+ a (* k h))))))
  (/ (sum term 0 inc n)
     3.0))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
; both results are 0.25, which are more accurate than integral