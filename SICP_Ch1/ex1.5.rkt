#lang sicp

; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; According to the text, if the interpreter works as applicative-order, it will first evaluate (p),
; which will enter a endless loop, because (p) will always call itself (p).
; If the interpreter works as normal order, it will test whether 0 == 9, since this is true,
; it will return 0, without checking (p), and avoid the endless loop