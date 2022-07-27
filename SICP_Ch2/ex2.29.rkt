#lang sicp
; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight-branch branch)
  (let ((w (branch-structure branch)))
    (if (pair? w)
        (total-weight-branch w)
        w)))

(define (total-weight-mobile mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

; mutural recursion
(define (balanced-branch? branch)
  (let ((b (branch-structure branch)))
    (if (pair? b)
        (balanced-mobile? b)
        #t)))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (balanced-branch? left)
         (balanced-branch? right)
         (= (* (total-weight-branch left) (branch-length left))
            (* (total-weight-branch right) (branch-length right))))))

(define a (make-branch 5 3))
(define b (make-mobile a (make-branch 1 (make-mobile a a))))
(define c (make-mobile (make-branch 1 (make-mobile a a)) (make-branch 1 (make-mobile a a))))

(total-weight-mobile b)
(total-weight-mobile c)
(balanced-mobile? b)
(balanced-mobile? c)

; d: only to change (define right-branch cdr) and (define branch-structure cdr)
