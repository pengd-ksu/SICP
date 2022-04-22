#lang sicp
(define (square x)
  (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)  ;; can't use let here, it could only define  non-procedure
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
   (* (rect-area x1 x2 y1 y2)
      (monte-carlo trials experiment)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (rect-area x1 x2 y1 y2)
  (abs (* (- x2 x1)
          (- y2 y1))))

(define (estimate-pi trials)
  (define (in-circle? x y)
    (<= (+ (square x) (square y)) 1.0))
  (estimate-integral in-circle? -1.0 1.0 -1.0 1.0 trials))

(estimate-pi 100000)