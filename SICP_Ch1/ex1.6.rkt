#lang sicp

;
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; if is special form that it only checks the else predict when if clause is false
; while cond will check all its branches. In this case sqrt-iter will loop forever