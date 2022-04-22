#lang racket

(define (square x)
  (* x x))

(define (cont-frac-recur n d k)
  (define (aux i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (aux (+ i 1))))))
  (aux 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (/ (n (- i 1))
                         (+ (d (- i 1))
                            result)))))
  (iter k (/ (n k)
             (d k))))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac-recur n d k))

;;;;;;;;;;;;;
; test case from:
; https://github.com/hjcapple/reading-sicp/blob/master/chapter_1/exercise_1_39.md
(module* test #f
  (require rackunit)
  (define (for-loop n last op)
    (cond ((<= n last)
           (op n)
           (for-loop (+ n 1) last op))))
  
  (define (test-n n)
    (let ((x (/ n (* 2 3.1415926))))
      (check-= (tan-cf x 100) (tan x) 0.0001)))
  
  (for-loop 0 360 test-n)
)