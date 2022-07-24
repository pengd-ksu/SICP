#lang sicp

(define (reverse items)
  (define (reverse-iter ls result)
    (if (null? ls)
        result
        (reverse-iter (cdr ls) (append (list (car ls)) result))))
  (reverse-iter items nil))

(reverse (list 1 4 9 16 25))