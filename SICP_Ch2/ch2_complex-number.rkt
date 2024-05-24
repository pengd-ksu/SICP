#lang sicp

(define (square x)
  (* x x))

; Section 2.4.1 Representations for Complext Numbers
(define (add-complex-Ben z1 z2)
  (make-from-real-imag-Ben (+ (real-part-Ben z1) (real-part-Ben z2))
                           (+ (imag-part-Ben z1) (imag-part-Ben z2))))

(define (sub-complex-Ben z1 z2)
  (make-from-real-imag-Ben (- (real-part-Ben z1) (real-part-Ben z2))
                           (- (imag-part-Ben z1) (imag-part-Ben z2))))

(define (mul-complex-Ben z1 z2)
  (make-from-mag-ang-Ben (* (magnitude-Ben z1) (magnitude-Ben z2))
                         (+ (angle-Ben z1) (angle-Ben z2))))

(define (div-complex-Ben z1 z2)
  (make-from-mag-ang-Ben (/ (magnitude-Ben z1) (magnitude-Ben z2))
                         (- (angle-Ben z1) (angle-Ben z2))))

(define (real-part-Ben z) (car z))

(define (imag-part-Ben z) (cdr z))

(define (magnitude-Ben z)
  (sqrt (+ (square (real-part-Ben z)) (square (imag-part-Ben z)))))

(define (angle-Ben z)
  (atan (imag-part-Ben z) (real-part-Ben z)))

(define (make-from-real-imag-Ben x y) (cons x y))

(define (make-from-mag-ang-Ben r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (real-part-Aly z)
  (* (magnitude-Aly z) (cos (angle-Aly z))))

(define (imag-part-Aly z)
  (* (magnitude-Aly z) (sin (angle-Aly z))))

(define (magnitude-Aly z) (car z))

(define (angle-Aly z) (cdr z))

(define (make-from-real-imag-Aly x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang-Aly r a) (cons r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum))