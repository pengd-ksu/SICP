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
; (my-car a)
; 3
; (my-cdr a)
; 5

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

;(define squares (list 1 4 9 16 25))
;(list-ref squares 3)

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
; (length-recur odds)
; 4
; (length-iter odds)
; 4

;(append squares odds)
; (1 4 9 16 25 1 3 5 7)
;(append odds squares)
;(1 3 5 7 1 4 9 16 25)

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

;(my-append squares odds)
;(my-append odds squares)

; Mapping over lists
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
;(scale-list (list 1 2 3 4 5) 10)
;(10 20 30 40 50)

(define (map-v1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
;(map-v1 abs (list -10 2.5 -11.6 17))
;(10 2.5 11.6 17)

;(map-v1 (lambda (x) (* x x)) (list 1 2 3 4))
;(1 4 9 16)

(define (scale-list-v1 items factor)
  (map-v1 (lambda (x) (* x factor))
          items))
;(scale-list-v1 (list 1 2 3 4 5) 10)
;(10 20 30 40 50)

; 2.2.2 Hierachical Structures
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Mapping over trees
(define (scale-tree-v1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree-v1 (car tree) factor)
                    (scale-tree-v1 (cdr tree) factor)))))
;(scale-tree-v1 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;(10 (20 (30 40) 50) (60 70))

(define (scale-tree-v2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-v2 sub-tree factor)
             (* sub-tree factor)))
       tree))
;(scale-tree-v2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;(10 (20 (30 40) 50) (60 70))

; 2.2.3 Sequences as Conventional Interfaces
(define (square x)
  (* x x))

(define (sum-odd-squares-v1 tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares-v1 (car tree))
                 (sum-odd-squares-v1 (cdr tree))))))

;(sum-odd-squares-v1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; 84

; fib from chapter 1
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (even-fibs-v1 n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;(fib 7)
;13
;(even-fibs-v1 7)
;(0 2 8)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;(filter odd? (list 1 2 3 4 5))
;(1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;(accumulate + 0 (list 1 2 3 4 5))
;15
;(accumulate * 1 (list 1 2 3 4 5))
;120
;(accumulate cons nil (list 1 2 3 4 5))
;(1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
;(enumerate-interval 2 7)
;(2 3 4 5 6 7)
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;(1 2 3 4 5)

(define (sum-odd-squares-v2 tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))
;(sum-odd-squares-v2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;84

(define (even-fibs-v2 n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))
;(fib 7)
;13
;(even-fibs-v2 7)
;(0 2 8)

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

;(list-fib-squares 10)
;(0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;225

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ;(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

;(prime-sum-pairs 6)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
;(permutations '(1 2 3))
;((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))