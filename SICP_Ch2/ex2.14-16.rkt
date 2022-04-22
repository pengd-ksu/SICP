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
  (let ((upper-x (upper-bound x))
        (lower-x (lower-bound x))
        (upper-y (upper-bound y))
        (lower-y (lower-bound y)))
    (cond ((and (>= upper-x 0) (>= lower-x 0)
                (>= upper-y 0) (>= lower-y 0))
           (make-interval (* lower-x lower-y) (* upper-x upper-y)))
          ((and (>= upper-x 0) (>= lower-x 0)
                (>= upper-y 0) (< lower-y 0))
           (make-interval (* upper-x lower-y) (* upper-x upper-y)))
          ((and (>= upper-x 0) (>= lower-x 0)
                (< upper-y 0) (< lower-y 0))
           (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((and (>= upper-x 0) (< lower-x 0)
                (>= upper-y 0) (>= lower-y 0))
           (make-interval (* lower-x upper-y) (* upper-x upper-y)))
          ((and (>= upper-x 0) (< lower-x 0)
                (>= upper-y 0) (< lower-y 0))
           (let ((p1 (* lower-x upper-y))
                 (p2 (* upper-x lower-y))
                 (p3 (* lower-x lower-y))
                 (p4 (* upper-x upper-y)))
             (make-interval (min p1 p2)
                            (max p3 p4))))
          ((and (>= upper-x 0) (< lower-x 0)
                (< upper-y 0) (< lower-y 0))
           (make-interval (* upper-x lower-y) (* lower-x lower-y)))
          ((and (< upper-x 0) (< lower-x 0)
                (>= upper-y 0) (>= lower-y 0))
           (make-interval (* lower-x upper-y) (* upper-x lower-y)))
          ((and (< upper-x 0) (< lower-x 0)
                (>= upper-y 0) (< lower-y 0))
           (make-interval (* lower-x upper-y) (* lower-x lower-y)))
          ((and (< upper-x 0) (< lower-x 0)
                (< upper-y 0) (< lower-y 0))
           (make-interval (* upper-x upper-y) (* lower-x lower-y))))))

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

(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
; test case from
; https://github.com/hjcapple/reading-sicp/blob/master/chapter_2/exercise_2_14.md
(define (print-interval v)
  (newline)
  (display "[")
  (display (lower-bound v))
  (display ", ")
  (display (upper-bound v))
  (display "]"))

(define A (make-center-percent 20 2))
(define B (make-center-percent 10 2))

(print-interval A)
(print-interval B)
(print-interval (part1 A B));part1 has a wider range than part2, that's because vairables are
; repeated in part1, which accumulates more errors.
(print-interval (part2 A B))

(print-interval (div-interval A A))
; R2/R2 != 1, this is the insight. Because every variable has a range

(print-interval (div-interval A B))
(print-interval (div-interval B B))