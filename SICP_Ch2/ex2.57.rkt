#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum . l) (list '+ l))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product . l) (append (list '*) (list l)))

(define (make-product-simplify m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend-original s) (caddr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (append (list '+) (cddr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand-original p) (caddr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (append (list '*) (cddr p))))

;;;;;;;;;;;;;;;;;;;;;
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ x x x) 'x)
(make-sum 'x 'y 'z)
(make-product 'x 'y 'z)