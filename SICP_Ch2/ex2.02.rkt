#lang sicp
;; Exercise 2.2

(define (average x y)
  (/ (+ x y)
     2.0))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line)
  (let ((start (start-segment line))
        (end (end-segment line)))
    (cons (average (x-point start)
                   (x-point end))
          (average (y-point start)
                   (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;;;;;;;;;;;;;;
(define seg0 (make-segment (make-point 10 10) (make-point 20 20)))
(define seg1 (make-segment (make-point 3 3) (make-point 4 4)))
(print-point (midpoint-segment seg0))
(print-point (midpoint-segment seg1))

