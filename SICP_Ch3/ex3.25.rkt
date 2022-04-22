#lang sicp
;; The easiest way might be using a list as key. Authors of the following link agree:
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/exercise_3_25_a.scm
;; https://eli.thegreenplace.net/2007/10/04/sicp-section-333

;; Just for one-dimensional table. Same value stored under arbitrary number of keys.

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Test kit from:
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/exercise_3_25_a.scm

(define t (make-table))
(insert! 'a 1 t)
(insert! 'b 2 t)
(insert! (list 'a 'b 'c) 3 t)

(lookup 'a t)
(lookup 'b t)
(lookup (list 'a 'b 'c) t)

(insert! (list 'a 'b 'c) 10 t)
(lookup (list 'a 'b 'c) t)


(insert! (list 'key1 'key2 'key3) 123 t)
(lookup (list 'key1 'key2 'key3) t)

; 修改三个键
(insert! (list 'key1 'key2 'key3) 'hello-world t)  
(lookup (list 'key1 'key2 'key3) t)