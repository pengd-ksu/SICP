#lang racket

(define (accumulate-recur combiner null-value term a next b)
  (if (a > b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a)
                                 result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(define (even? n)
  (= (remainder n 2) 0))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test #f
  (require rackunit)
  (check-equal? (sum identity 1 inc 100) 5050)
  (check-equal? (product identity 1 inc 6) 720)
)