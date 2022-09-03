#lang sicp

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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

;ex 2.66
(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (value record)
  (cdr record))

(define (lookup-bst given-key bst)
  (cond ((null? bst) false)
        ((= given-key (key (entry bst)))
         (value (entry bst)))
        ((< given-key (key (entry bst)))
         (lookup-bst given-key (left-branch bst)))
        (else
         (lookup-bst given-key (right-branch bst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define binary-tree (list->tree '((1 "Yellow")
                                  (2 "Blue")
                                  (3 "Red")
                                  (4 "White")
                                  (9 "Green"))))

(lookup-bst 9 binary-tree)
(lookup-bst 12 binary-tree)
(lookup-bst 2 binary-tree)