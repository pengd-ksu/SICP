#lang racket

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test-all n)
  (define (try-it-all a)
    (if (= a n)
        true
        (and (= (expmod a n n) a)
             (try-it-all (+ a 1)))))
  (try-it-all 1))
; If reverse the order of (not (prime? n)) and (fermat-test-all n), it will be much slower.
; Because prime? is much faster. 
(define (search-carmichael n count)
  (cond ((not (= count 0))
         ;(cond ((and (fermat-test-all n) (not (prime? n)))
         (cond ((and (not (prime? n)) (fermat-test-all n))
                (displayln n)
                (search-carmichael (+ n 1) (- count 1)))
               (else (search-carmichael (+ n 1) count))))))

(search-carmichael 1 8);display the first ten Carmichael numbers starting from 1