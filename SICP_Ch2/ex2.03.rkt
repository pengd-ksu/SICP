#lang sicp


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

; Exercise 2.3
; Representation 1: p1 and p2 are vertically parallel segments of a rectangle
(define (make-rect-p p1 p2)
  (cons p1 p2))
(define (rect-width-p rect)
  (abs (- (x-point (start-segment (car rect)))
          (x-point (start-segment (cdr rect))))))
(define (rect-height-p rect)
  (abs (- (y-point (start-segment (car rect)))
          (y-point (end-segment (cdr rect))))))
; Representation 2: p1 and p2 are diagonal segments of a rectangle
(define (make-rect-d p1 p2)
  (cons p1 p2))
(define (rect-width-d rect)
  (abs (- (x-point (start-segment (car rect)))
          (x-point (end-segment (car rect))))))
(define (rect-height-d rect)
  (abs (- (y-point (start-segment (car rect)))
          (y-point (end-segment (car rect))))))

(define (rect-perimeter-d rect)
  (+ (* 2 (rect-width-d rect))
     (* 2 (rect-height-d rect))))
(define (rect-area-d rect)
  (* (rect-width-d rect)
     (rect-height-d rect)))

(define (rect-perimeter-p rect)
  (+ (* 2 (rect-width-p rect))
     (* 2 (rect-height-p rect))))
(define (rect-area-p rect)
  (* (rect-width-p rect)
     (rect-height-p rect)))

(define re-d (make-rect-d (make-segment (make-point 2 2) (make-point 4 4))
                          (make-segment(make-point 4 2) (make-point 2 4))))
(rect-width-d re-d)
(rect-perimeter-d re-d)
(rect-area-d re-d)

(define re-p (make-rect-p (make-segment (make-point 2 2) (make-point 2 4))
                          (make-segment(make-point 4 2) (make-point 4 4))))
(rect-width-p re-d)
(rect-perimeter-p re-p)
(rect-area-p re-p)
