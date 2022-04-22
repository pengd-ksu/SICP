#lang sicp

;a=bq+aq+ap, b=bp+ap, apply T
;a=(bp+aq)+(bq+ap+ap)q+(bq+aq+ap)p=b(qq+2pq)+a(qq+2pq)+a(pp+qq)
;therefore, q'=qq+2pq, p'=pp+qq
;Could also get inspiration from fibonacci matrix form, check wikipedia, and
;https://github.com/hjcapple/reading-sicp/blob/master/chapter_1/exercise_1_19.md
(define (square x)
  (* x x))
; Exercise 1.19
(define (fib n)
  (fib-iteration 1 0 0 1 n))
(define (fib-iteration a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iteration a
                        b
                        (+ (square p) (square q))
                        (+ (square q) (* 2 p q))
                        (/ count 2)))
        (else (fib-iteration (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))