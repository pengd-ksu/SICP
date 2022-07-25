; Exercise 2.22
#lang sicp

(define (square x)
  (* x x))

(define (square-list-v1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-v2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list-v1 (list 1 3 5 7 9))
(square-list-v2 (list 1 3 5 7 9))