; Exercise 2.20
#lang sicp

(define (same-parity first . rest)
  (define (same-parity-iter ls result)
    (cond ((null? ls) result)
          ((even? (- first (car ls)))
           (same-parity-iter (cdr ls) (append result (list (car ls)))))
          (else (same-parity-iter (cdr ls) result))))
  (same-parity-iter rest (list first)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)