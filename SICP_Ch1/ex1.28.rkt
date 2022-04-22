#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (expmod-MR base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((candidate (expmod-MR base (/ exp 2) m))
                (root (remainder (square candidate) m)))
           (if (and (not (= candidate 1))
                    (not (= candidate (- m 1)))
                    (= root 1))
               0
               root)))
        (else
         (remainder (* base (expmod-MR base (- exp 1) m))
                    m))))

(define (fermat-test-MR n)
  (define (try-it a)
    (= (expmod-MR a (- n 1) n) 1)); if equals to 0, then n is not prime
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test-MR n)
         (fast-prime? n (- times 1)))
        (else false)))

(module* test #f
  (require rackunit)
  (for-each (lambda (num)
              (check-true (fast-prime? num 100)))
            '(2 3 5 7 11 13 17 19 23))
  (for-each (lambda (num)
              (check-false (fast-prime? num 100)))
            '(36 25 9 16 4 561 1105 1729 2465 2821 6601 8911 10585))
)

;first eight carmichael: 561, 1105, 1729, 2465, 2821, 6601, 8911, 10585


