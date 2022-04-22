#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

; Exercise 1.18
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-mul-iter a b)
  (define (aux-iter a b ans)
    (cond ((= b 0) ans)
          ((even? b) (aux-iter (double a) (halve b) ans))
          (else (aux-iter a (- b 1) (+ ans a)))))
  (aux-iter a b 0))