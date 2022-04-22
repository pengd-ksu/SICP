#lang racket

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (* (term a)
                          result))))
  (iter a 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (wallis-pi product-f n)
  (define (wallis-term k)
    (let ((nom (if (even? k)
                   (+ k 2.0)
                   (+ k 1)))
          (denom (if (even? k)
                     (+ k 1)
                     (+ k 2))))
      (/ nom denom)))
  (* (product-f wallis-term 1 inc n)
     4))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product-iter identity 1 inc n))

;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)

(check-equal? (factorial 6) 720)
(check-= (wallis-pi product-recur 1000) (wallis-pi product-iter 1000) 0.0001)