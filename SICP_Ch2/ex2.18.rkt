#lang sicp

(define (reverse items)
  (define (iter items ls)
    (if (null? items)
        ls
        (iter (cdr items) (cons (car items) ls))))
  (iter items nil))

(reverse (list 1 4 9 16 25))