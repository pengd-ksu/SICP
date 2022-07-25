; Exercise 2.21
#lang sicp

(define (square x)
  (* x x))

(define (square-list-v1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-v1 (cdr items)))))

(define (square-list-v2 items)
  (map (lambda (x) (square x))
       items))

(square-list-v1 (list 1 3 5 7 9))
(square-list-v2 (list 1 3 5 7 9))