#lang racket

(define (filtered-accumulate-recur filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate-recur filter combiner null-value term (next a) next b))
          (filtered-accumulate-recur filter combiner null-value term (next a) next b))))

(define (filtered-accumulate-iter filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a)
                                     result))
            (iter (next a) result))))
  (iter a null-value))

(define (even? n)
  (= (remainder n 2) 0))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum-squares-of-primes a b)
  (filtered-accumulate-iter prime? + 0 square a inc b))

(define (product-of-relatively-prime n)
  (define (filter-gcd i)
    (= (gcd i n) 1))
  (filtered-accumulate-iter filter-gcd * 1 identity 1 inc (- n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test #f
  (require rackunit)
  (check-equal? (sum-squares-of-primes 2 10) (+ 4 9 25 49))
  (check-equal? (sum-squares-of-primes 2 20) (+  4 9 25 49 121 169 289 361))
  
  (check-equal? (product-of-relatively-prime 10) (* 1 3 7 9))
  (check-equal? (product-of-relatively-prime 20) (* 1 3 7 9 11 13 17 19))
)