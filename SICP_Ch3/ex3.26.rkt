#lang sicp
(define (make-tree key value left right)
  (cons (cons key value) (cons left right)))

(define (tree-key tree) (car (car tree)))

(define (tree-value tree) (cdr (car tree)))

(define (left-branch tree) (car (cdr tree)))

(define (right-branch tree) (cdr (cdr tree)))

(define (set-value! tree value) (set-cdr! (car tree) value))

(define (set-left! tree left) (set-car! (cdr tree) left))

(define (set-right! tree right) (set-cdr! (cdr tree) right))

(define (make-table how-to-compare)
  (let ((local-table '()))
    (define (lookup-tree key tree)
      (if (null? tree)
          false
          (let ((compare-result (how-to-compare key (tree-key tree))))
            (cond ((eq? compare-result '=) (tree-value tree))
                  ((eq? compare-result '>) (lookup-tree key (right-branch tree)))
                  ((eq? compare-result '<) (lookup-tree key (left-branch tree)))
                  (else (error "Unknown comparing result" compare-result))))))
    (define (insert-tree! key value tree)
      (if (null? tree)
          (make-tree key value '() '())
       ;;   (begin (set! tree (make-tree key value '() '())) ;; doesn't work. local-table remains '()
       ;;          tree)
          (let ((compare-result (how-to-compare key (tree-key tree))))
            (cond ((eq? compare-result '=)
                   (set-value! tree value))
                  ((eq? compare-result '>)
                   (set-right! tree (insert-tree! key value (right-branch tree))))
                  ((eq? compare-result '<)
                   (set-left! tree (insert-tree! key value (left-branch tree))))
                  (else (error "Unknown comparing result" compare-result)))
            tree)))
    (define (lookup key)
      (lookup-tree key local-table))
    (define (insert! key value)
      (set! local-table (insert-tree! key value local-table)))
   ;;   (insert-tree! key value local-table)) ;; start from '(), must set itself as a value.
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknow operation -- TABLE" m))))
    dispatch))

(define (compare-number key-1 key-2)
  (cond ((= key-1 key-2) '=)
        ((> key-1 key-2) '>)
        ((< key-1 key-2) '<)))

(define tree-table
  (make-table compare-number))

(define (insert! key value)
  ((tree-table 'insert-proc!) key value))

(define (lookup key)
  ((tree-table 'lookup-proc) key))

(insert! 1 "One")
(insert! 9 "Nine")
(insert! 6 "Six")
(insert! 3 "Three")
(insert! 7 "Seven")

(lookup 9)