#lang racket

;Exercise 1.11
(define (f_rec n)
  (cond ((< n 3) n)
        (else (+ (f_rec (- n 1))
                 (* (f_rec (- n 2)) 2)
                 (* (f_rec (- n 3)) 3)))))

(define (f_iter n)
  (define (aux f1 f2 f3 count)
    (if (= count 2)
        f1
        (aux (+ f1
                (* 2 f2)
                (* 3 f3))
             f1
             f2
             (- count 1))))
  (if (< n 3)
      n
      (aux 2 1 0 n)))

(require rackunit)
(define (for-loop n last op)
  (cond ((<= n last)
         (op n)
         (for-loop (+ n 1) last op))))

(define (test-n n)
  (check-equal? (f_rec n) (f_iter n))
  (displayln (f_rec n)))

(for-loop 0 16 test-n)