#lang sicp

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -)
   a
   b))
; a will plus absolute value of b, because the operator will be + if b > 0,
; and - if b <= 0