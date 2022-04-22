#lang sicp

; Counting change continuing from section 1.2.2
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount coin-values)
  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
           (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values))
                  coin-values)))))
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cc amount coin-values))

(count-change 100 us-coins)
(count-change 100 (list 25 5 10 50 1))
; The order doesn't affect the final results, because count-change will check all the possibilities,
; whatever their orders are