#lang sicp

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))
; Exercise 1.16
(define (fast-expt-iter b n)
  (define (aux-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (aux-expt-iter a (square b) (/ n 2)))
          (else (aux-expt-iter (* a b) b (- n 1)))))
  (aux-expt-iter 1 b n))