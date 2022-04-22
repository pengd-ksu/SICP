#lang sicp
(define (count-pairs x)
  (let ((record '()))
    (define (count-pairs-help x)
      (cond ((not (pair? x)) 0)
            ((memq x record) 0)
            (else (set! record (cons x record))
                  (+ (count-pairs-help (car x))
                     (count-pairs-help (cdr x))
                     1))))
    (count-pairs-help x)))

;; The following code is wrong, because in every recursive procedure, it estalbishes
;; a new record, which is null. Therefore, it can't remember anything.
(define (count-pairs-wrong x) 
  (let ((record '()))
    (cond ((not (pair? x)) 0)
          ((memq x record) 0)
          (else (set! record (cons x record))
                (+ (count-pairs-wrong (car x))
                   (count-pairs-wrong (cdr x))
                   1)))))

(define z1 (cons 'a (cons 'b (cons 'c nil))))
(count-pairs z1)
;; (count-pairs-wrong z1)

(define x2 (cons 'a 'b))
(define z2 (cons 'c (cons x2 x2)))
(count-pairs z2)
;; (count-pairs-wrong z2)

(define x3 (cons 'a 'b))
(define y3 (cons x3 x3))
(define z3 (cons y3 y3))
(count-pairs z3)
;; (count-pairs-wrong z3)

(define z4 (list 'a 'b 'c))
(set-cdr! (cddr z4) z4)
(count-pairs z4)