#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y))
                  (- (lower-bound y)))))

(define (mul-interval x y)
  (let((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "division error (interval spans 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (let((c (center i))
       (w (width i)))
    (abs (* (/ w c) 100.0))));such as 70, meaning 70%

(define (make-center-percent c p)
  (make-interval (- c (* (abs c) (/ p 100.0)))
                 (+ c (* (abs c) (/ p 100.0)))))

; test case from
; https://github.com/hjcapple/reading-sicp/blob/master/chapter_2/exercise_2_7.scm
(define (print-interval v)
  (newline)
  (display "[")
  (display (lower-bound v))
  (display ", ")
  (display (upper-bound v))
  (display "]"))

(define c (make-center-percent 10 20))
(print-interval c)
(newline)
(center c)
(percent c)