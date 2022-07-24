#lang sicp

; Counting change continuing from section 1.2.2
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount coin-list)
  (define (cc amount coin-list)
    (cond ((=  amount 0) 1)
          ((or (< amount 0) (no-more? coin-list)) 0)
          (else (+ (cc amount
                       (except-first-denomination coin-list))
                   (cc (- amount
                          (first-coin coin-list))
                       coin-list)))))
  (define (no-more? coin-list)
    (null? coin-list))
  (define (first-coin coin-list)
    (car coin-list))
  (define (except-first-denomination coin-list)
    (cdr coin-list))
  (cc amount coin-list))

(count-change 100 us-coins)
(count-change 100 (list 25 5 10 50 1))
(count-change 100 uk-coins)
; The order doesn't affect the final results, because count-change will
; check all the possibilities, whatever their orders are