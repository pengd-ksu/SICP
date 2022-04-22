#lang racket

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recur term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ (term a)
                          result))))
  (iter a 0))

(module* test #f
  (require rackunit)
  (define (inc n) (+ n 1))
  (define (identity x) x)
  
  (check-equal? (sum-iter identity 1 inc 100) 5050)
)
