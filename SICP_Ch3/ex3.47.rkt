#lang sicp
;; Section 3.4.2
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))  ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; Exercise 3.47
;; mutex is to protect counter to be in a consistent state
(define (make-semaphore_a n)
  (let ((mutex (make-mutex)) 
        (count n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> count 0)
                 (begin (set! count (- count 1))
                              (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))))   ; retry
            ((eq? m 'release)
             (mutex 'acquire)
             (if (<= count n)
                 (begin (set! count (+ count 1))
                        (mutex 'release))
                 (mutex 'release)))))
    the-semaphore))

;; In terms of test-and-set!
(define (make-semaphore_b n)
  (let ((cell (list false))
        (count n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)
                 (if (> count 0)
                     (begin (set! count (- count 1))
                            (clear! cell))
                     (begin (clear! cell)
                            (the-semaphore 'acquire)))))
            ((eq? m 'release)
             (if (test-and-set! cell) ;; atomic to protect count action from interleaving
                 (the-semaphore 'release)
                 ((if (<= count n) ;; no other procedure could enter here
                      (set! count (+ count 1)))
                  (clear! cell))))))
    the-semaphore))