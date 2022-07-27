#lang sicp
; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (fringe (car tree))
                              (fringe (cdr tree))))
        (else (list tree))))

(define x1 (list (list 1 2) (list 3 4)))
(define x2 (list 1 4 9 16 25))

(fringe x1)
(fringe x2)
(fringe (list x1 x1))
