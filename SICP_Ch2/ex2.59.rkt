#lang racket

;ex 2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s1 '(1 3 5 7 9 11 13))
(define s2 '(2 3 5 6 7 8 9))

(element-of-set? 6 s1)
(element-of-set? 9 s1)

(adjoin-set 10 s1)
(intersection-set s1 s2)
(union-set s1 s2)
