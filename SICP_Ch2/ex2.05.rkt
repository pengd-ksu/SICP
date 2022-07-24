#lang sicp

(define (expt-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (expt-car x)
  (define (iter x a)
    (if (= (remainder x 2) 0)
        (iter (/ x 2) (+ a 1))
        a))
  (iter x 0))

(define (expt-cdr x)
  (define (iter x b)
    (if (= (remainder x 3) 0)
        (iter (/ x 3) (+ b 1))
        b))
  (iter x 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(expt-cons 2 3)
(expt-car (expt-cons 10 20))
(expt-cdr (expt-cons 10 20))