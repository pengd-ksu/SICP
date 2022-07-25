#lang sicp

; greated common divisor of two integers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;(define (make-rat n d)
;  (cons n d))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; 2.1.3 What Is Meant by Data?
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (my-car z)
  (z 0))
(define (my-cdr z)
  (z 1))

(define a (my-cons 3 5))
(my-car a)
(my-cdr a)

; 2.1.4 Extended Exercise: Interval Arithmetic
(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y))
                  (- (lower-bound y)))))

(define (mul-interval x y)
  (let((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "division error (interval spans 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (let((c (center i))
       (w (width i)))
    (abs (* (/ w c) 100.0))));such as 70, meaning 70%

(define (make-center-percent c p)
  (make-interval (- c (* (abs c) (/ p 100.0)))
                 (+ c (* (abs c) (/ p 100.0)))))

; 2.2.1 Representing Sequences
(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length-recur items)
  (if (null? items)
      0
      (+ 1 (length-recur (cdr items)))))

(define (length-iter items)
  (define (iter ls count)
    (if (null? ls)
        count
        (iter (cdr ls) (+ 1 count))))
  (iter items 0))

(define odds (list 1 3 5 7))
(length-recur odds)
(length-iter odds)

(append squares odds)
(append odds squares)

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(my-append squares odds)
(my-append odds squares)

; Mapping over lists
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(scale-list (list 1 2 3 4 5) 10)

(define (map-v1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(map-v1 abs (list -10 2.5 -11.6 17))
(map-v1 (lambda (x) (* x x)) (list 1 2 3 4))

(define (scale-list-v1 items factor)
  (map-v1 (lambda (x) (* x factor))
          items))
(scale-list-v1 (list 1 2 3 4 5) 10)