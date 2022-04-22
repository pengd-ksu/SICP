#lang sicp
;; Section 1.2.6
;; prime?
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime?-1 n)
  (= n (smallest-divisor n)))

;; Section 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; Check reference: https://stackoverflow.com/questions/24529271/sicp-cons-stream
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;; Section 3.5.1
(define (sum-primes-1 a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime?-1 count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes-2 a b)
  (accumulate +
              0
              (filter prime?-1 (enumerate-interval a b))))

;; (car (cdr (filter prime? (enumerate-interval 10000 1000000)))) ;; too long time

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; Section 3.5.2
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;;(stream-ref no-sevens 100) ;; 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes-1 (sieve (integers-starting-from 2)))

;;(stream-ref primes-1 50) ;; 233

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers-imp (cons-stream 1 (add-streams ones integers-imp)))

(define fibs-imp
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs-imp))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes-2
  (cons-stream
   2
   (stream-filter prime?-2 (integers-starting-from 3))))

(define (prime?-2 n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes-2))

;; The rest of this chapter would be left for future

;; Section 3.5.5

;; Section 3.1.2
;; Both radom-init and rand-update refers to 
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/monte_carlo.scm
(define random-init 7)

(define (rand-update x)
  (let ((a 48271) (b 19851020) (m 2147483647))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; Resume section 3.5.5
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p)
                (if (= p 0)
                    0
                    (sqrt (/ 6 p))))
              (monte-carlo cesaro-stream 0 0)))

;;(stream-ref pi 5000)

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))

;; Section 3.5.3
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
;; (stream-head->list (sqrt-stream 2) 10)

;; Exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
                (add-streams (partial-sums s) (stream-cdr s))))

;; Resume section 3.5.3
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; The stream-head->list is from:
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/infinite_stream.scm
(define (stream-head->list s n)
  (if (or (= n 0) (stream-null? s))
      '()
      (cons (stream-car s) (stream-head->list (stream-cdr s) (- n 1)))))
;; (stream-head->list pi-stream 10)

;; Sequence accelerator of Leonhard Euler
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
;; (stream-head->list (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
;; (stream-head->list (accelerated-sequence euler-transform pi-stream) 10)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triple-set (triples integers integers integers))

(define pythagorean
  (stream-filter
   (lambda (x)
     (= (+ (square (car x))
           (square (cadr x)))
        (square (caddr x))))
   triple-set))

(stream-ref pythagorean 0)  ;(3 4 5)
(stream-ref pythagorean 1)  ;(6 8 10)
(stream-ref pythagorean 2)  ;(5 12 13)
(stream-ref pythagorean 3)  ;(9 12 15)
(stream-ref pythagorean 4)  ;(8 15 17)