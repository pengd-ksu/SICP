#lang sicp

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
        ((< x (entry set))
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

;ex 2.62
(define (intersection-order-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-order-set (cdr set1)
                                                (cdr set2))))
              ((< x1 x2)
               (intersection-order-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-order-set set1 (cdr set2)))))))

(define (union-order-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-order-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-order-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-order-set set1 (cdr set2)))))))))

;ex 2.63
(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list
                     (right-branch tree))))))

;ex 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; set1 and set2 are both balanced-tree sets
(define (union-set set1 set2)
  (list->tree (union-order-set (tree->list set1)
                               (tree->list set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-order-set (tree->list set1)
                                      (tree->list set2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a (list->tree '(2 4 6 8 10)))
(define b (list->tree '(3 4 5 6 7 8 9)))

(element-of-tree-set? 3 a)
(element-of-tree-set? 9 b)

(adjoin-tree-set 7 a)
(adjoin-tree-set 10 a)
(adjoin-tree-set 11 a)
(intersection-set a b)
(union-set a b)