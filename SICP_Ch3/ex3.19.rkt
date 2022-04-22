#lang sicp
;; Cycle detection: https://en.wikipedia.org/wiki/Cycle_detection#CITEREFJoux2009
(define (contain-cycle? x)
  (define (contain-cycle?-help x1 x2)
    (cond ((not (pair? x2)) false)
          ((not (pair? (cdr x2))) false)
          ((eq? x1 x2) true)
          (else
           (contain-cycle?-help (cdr x) (cddr x2)))))
  (if (not (pair? x)) ;; already contains x1 not pair?
      false
      (contain-cycle?-help x (cdr x))))

;;;;;;;;;;;;;;;;;;;;;;;;
(define (last-pair x)
  (if (null? (cdr x))
      x 
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;;;;;;;;;;;;;;;;;;;;;;;;;
(contain-cycle? (list 'a 'b 'c 'd))              ; #f

(contain-cycle? (make-cycle (list 'a)))          ; #t
(contain-cycle? (make-cycle (list 'a 'b 'c)))    ; #t
(contain-cycle? (make-cycle (list 'a 'b 'c 'd))) ; #t

(contain-cycle? (cons 1 2))                      ; #f
(contain-cycle? '(1))                            ; #f
(contain-cycle? '())                             ; #f

