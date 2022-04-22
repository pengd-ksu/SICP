#lang sicp

; Exercise 1.25
; The original expmod used a math trick to keep numbers down. While Hacker's method
; will still used the original number, and when it's big, the pc will encounter problems
; to perform on it. Considering that the primes are usually applied in security area involving
; large numbers, it's necessary to keep the procedure under a certain precision.
; The original implementatin will involve procedure (square (expmod base (/ exp 2) m)) and
; (* base (expmod base (- exp 1) m), and largest number of interest won't be larger than
; m * m and base * m
(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod_Hacker base exp m)
  (remainder (fast-expt base exp) m))

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