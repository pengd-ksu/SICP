#lang sicp
; Exercise 2.30

(define (square x)
  (* x x))

(define (square-tree-v1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-v1 (car tree))
                    (square-tree-v1 (cdr tree))))))

(define (square-tree-v2 tree)
  (define (f sub-tree)
    (if (pair? sub-tree)
        (square-tree-v2 sub-tree)
        (square sub-tree)))
  (map f tree))

(define tree '(1 (2 (3 4) 5 (6 7))))
(square-tree-v1 tree)
(square-tree-v2 tree)