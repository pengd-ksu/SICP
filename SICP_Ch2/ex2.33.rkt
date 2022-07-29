#lang sicp
; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define ls1 (list 1 2 3 4 5))
(define ls2 (list 6 7 8 9))
(define (square x)
  (* x x))

(my-map square ls1)
(my-append ls1 ls2)
(my-length ls1)
(my-length ls2)