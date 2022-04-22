#lang sicp
;; The following code is from:
;; https://eli.thegreenplace.net/2007/09/27/sicp-sections-312-313
(define f
  (let ((state 1))
    (lambda (n)
      (set! state (* state n))
      state)))

(+ (f 0) (f 1))
;; The result is 0, it seems that the '+' executed from left to right