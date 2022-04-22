#lang sicp
;; Section 3.3.1
(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

z1
z2

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
(set-to-wow! z1)
(set-to-wow! z2)

(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;; Mutation is just assignment
(define (cons-v1 x y)
  (define (dispatch m)
    (cond ((eq? m 'car-v1) x)
          ((eq? m 'cdr-v1) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
((cons-v1 'a 'b) 'car-v1)
((cons-v1 'a 'b) 'cdr-v1)

(define (car-v1 z) (z 'car-v1))
(define (cdr-v1 z) (z 'cdr-v1))
(car-v1 (cons-v1 'c 'd))
(cdr-v1 (cons-v1 'c 'd))

(define (cons-v2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car-v2) x)
          ((eq? m 'cdr-v2) y)
          ((eq? m 'set-car!-v2) set-x!)
          ((eq? m 'set-cdr!-v2) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car-v2 z) (z 'car-v2))
(define (cdr-v2 z) (z 'cdr-v2))
(define (set-car!-v2 z new-value)
  ((z 'set-car!-v2) new-value)
  z)
(define (set-cdr!-v2 z new-value)
  ((z 'set-cdr!-v2) new-value)
  z)

(car-v2 (cons-v2 'w 'z))
(cdr-v2 (cons-v2 'w 'z))
(define a-pair (cons-v2 'w 'z))
(car-v2 (set-car!-v2 a-pair 'u))
(cdr-v2 (set-cdr!-v2 a-pair 'v))
