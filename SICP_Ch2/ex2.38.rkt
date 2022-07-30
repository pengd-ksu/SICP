#lang sicp
; Exercise 2.38
; Check the example to understand fold-left and fold-right:
;(accumulate + 0 (list 1 2 3 4 5))
;(accumulate * 1 (list 1 2 3 4 5))
;(accumulate cons nil (list 1 2 3 4 5))
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

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(fold-right * 1 (list 2 3 5))
(fold-left * 1 (list 2 3 5))
(fold-right + 0 (list 2 3 5))
(fold-left + 0 (list 2 3 5))
; The results will be the same only if (op x y) == (op y x)
; Associative.