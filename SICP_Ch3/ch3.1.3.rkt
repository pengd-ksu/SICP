#lang sicp
;; Section 3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

(W 20)

(W 10)

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(D 20)

(D 10)

(define (factorial n) ;; illustrating that order of set! product and counter matters!
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))  
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))