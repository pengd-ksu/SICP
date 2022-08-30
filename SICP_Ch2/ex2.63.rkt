#lang sicp

;ex 2.63
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))
(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))
;(adjoin-tree-set 6 (list 7 (list 3 (list 1 nil nil) (list 5 nil nil)) (list 9 nil (list 11 nil nil))))
;(7 (3 (1 () ()) (5 () (6 () ()))) (9 () (11 () ())))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;a. Yes, both would produce in-order list for the given tree.
;b. Both are O(n).