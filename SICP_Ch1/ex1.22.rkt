#lang sicp

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))));(else (find-divisor n (next test-divisor))))) ;
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Exercise 1.22
(define (timed-prime-test n);delete (newline) and (display n) to only show primes
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time);slight modification to return boolean value
  (if (prime? n)
      (begin
        (report-prime n (- (runtime) start-time))
        #t)
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (cond ((even? start)
         (search-for-primes (+ start 1) count))
        (else
         (if (not (= count 0))
             (if (timed-prime-test start)
                 (search-for-primes (+ start 2) (- count 1))
                 (search-for-primes (+ start 2) count))))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)