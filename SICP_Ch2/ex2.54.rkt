#lang racket

(define (my_equal? l1 l2)
  (cond ((and (symbol? l1)
              (symbol? l2))
         (eq? l1 l2))
        ((and (null? l1)
              (null? l2))
         #t)
        ((and (pair? l1)
              (pair? l2))
         (and (my_equal? (car l1)
                         (car l2))
              (my_equal? (cdr l1)
                         (cdr l2))))
        (else #f)))

(my_equal? '(this is a list) '(this is a list))
(my_equal? '(this is a list) '(this (is a) list))