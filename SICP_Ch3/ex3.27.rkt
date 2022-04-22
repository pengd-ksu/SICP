#lang sicp
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)

;; If memo-fib is defined as (memoize fib), then fib (n) would call fib (n-1) and
;; fib (n-2), which are not stored in table. While memo-fib would call memo-fib (n-1)
;; and memo-fib (n-2), which have their own tables

;; The following code refers to following website, to check the procedure
;;https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/exercise_3_27.md
(define (fib n)
  (display "call fib ")
  (display n)
  (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib-2 (memoize fib))
(memo-fib-2 5)
;; (fib 3), (fib 2), (fib 1) are all called more than once.