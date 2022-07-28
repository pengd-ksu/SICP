#lang sicp
; Exercise 2.31

(define (square x)
  (* x x))

(define (tree-map-v1 proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map-v1 proc (car tree))
                    (tree-map-v1 proc (cdr tree))))))

(define (square-tree-v1 tree)
  (tree-map-v1 square tree))

(define (tree-map-v2 proc tree)
  (define (f sub-tree)
    (if (pair? sub-tree)
        (tree-map-v2 proc sub-tree)
        (proc sub-tree)))
  (map f tree))

(define (square-tree-v2 tree)
  (tree-map-v2 square tree))

(define tree '(1 (2 (3 4) 5 (6 7))))
(square-tree-v1 tree)
(square-tree-v2 tree)