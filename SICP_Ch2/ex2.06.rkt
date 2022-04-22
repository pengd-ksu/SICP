#lang sicp

; (add-1 zero) ->
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ->
; (lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ->
; (lambda (f) (lambda (x) (f x)))

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (inc x)
  (+ x 1))

((zero inc) 0)
((one inc) 0)
((two inc) 0)

(define (church-add a b)
  (lambda (f)
    (lambda (x)
      ((a f)((b f) x))))) ; appling b of f on x, then pass the result to a of f

(((church-add two two) inc) 0)

;; the following test cases are from:
; https://github.com/hjcapple/reading-sicp/blob/master/chapter_2/exercise_2_6.md
(define (make-church n)
  (define zero 
    (lambda (f) (lambda (x) x)))

  (define (add-1 a)
    (lambda (f) (lambda (x) (f ((a f) x)))))

  (if (= n 0) 
    zero
    (add-1 (make-church (- n 1)))))

(define (int-from-church n)
  (define (inc x) (+ x 1))
  ((n inc) 0))

;;;;;;;;;;;;;;;;;;;;
(define a (make-church 10))
(define b (make-church 20))
(define c (church-add a b))

(int-from-church a)
(int-from-church b)
(int-from-church c)