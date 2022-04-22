#lang sicp

(define (square x)
  (* x x))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.22
(define (timed-prime-test n);delete (newline) and (display n) to only show primes
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time);slight modification to return boolean value
  (if (fast-prime? n 100)
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