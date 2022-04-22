#lang sicp
(define (contain-cycle? x)
  (let ((record '()))
    (define (contain-cycle?-help x)
      (cond ((not (pair? x)) false)  ;; This will diminish repeated list!
            ((memq x record) true)
            (else (set! record (cons x record))
                  (contain-cycle?-help (cdr x)))))
    (contain-cycle?-help x)))

(define z1 (list 'a 'b 'c))
(set-cdr! (cddr z1) z1)
(contain-cycle? z1)
(define z2 (list 'a 'b 'c 'd))
(contain-cycle? z2)

