#lang sicp
; Exercise 2.27

(define (reverse items)
  (define (reverse-iter ls result)
    (if (null? ls)
        result
        (reverse-iter (cdr ls) (append (list (car ls)) result))))
  (reverse-iter items nil))

(define (reverse-deep items)
  (define (reverse-iter ls result)
    (if (null? ls)
        result
        (if (pair? (car ls))
            (reverse-iter (cdr ls) (cons (reverse-deep (car ls)) result))
            (reverse-iter (cdr ls) (append (list (car ls)) result)))))
  (reverse-iter items nil))

(define (reverse-deep-v2 items)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (append (reverse-deep-v2 (cdr items))
                      (list (reverse-deep-v2 (car items)))))))

(reverse (list 1 4 9 16 25))
(reverse (list (list 1 2) (list 3 4)))

(reverse-deep (list 1 4 9 16 25))
(reverse-deep (list (list 1 2) (list 3 4)))

(reverse-deep-v2 (list 1 4 9 16 25))
(reverse-deep-v2 (list (list 1 2) (list 3 4)))