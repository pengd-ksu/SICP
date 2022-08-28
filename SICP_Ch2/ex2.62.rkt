#lang racket

;ex 2.61
(define (element-of-order-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-order-set? x (cdr set)))))

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

(define (adjoin-order-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        (else (cons (car set)
                    (adjoin-order-set x (cdr set))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a '(2 4 6 8 10))
(define b '(3 4 5 6 7 8 9))

(element-of-order-set? 3 a)
(element-of-order-set? 9 b)

(adjoin-order-set 7 a)
(adjoin-order-set 10 a)
(adjoin-order-set 11 a)
(intersection-order-set a b)
(union-order-set a b)
