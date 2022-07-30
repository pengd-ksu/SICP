#lang sicp
; Exercise 2.39

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse-v1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse-v1 (list 1 2 3 4 5))

(define (reverse-v2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse-v2 (list 1 2 3 4 5))