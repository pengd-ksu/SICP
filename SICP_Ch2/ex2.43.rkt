#lang sicp
; Exercise 2.43

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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
          (lambda (positions) (safe? k positions))
          (flatmap
              (lambda (new-row)                   
                (map (lambda (rest-of-queens)
                      (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define row car)

(define col cdr)

(define (same-row? position1 position2)
  (= (row position1)
     (row position2)))

(define (same-diagnol? position1 position2)
  (= (abs (- (row position1) (row position2)))
     (abs (- (col position1) (col position2)))))

; column is from 1 to k (both ends inclusive)
(define (safe? k positions)
  (let* ((pair_list (map cons positions (enumerate-interval 1 k))); back to (row, col)
         (val (car pair_list)))
    (= (length (filter (lambda (x)
                         (or (same-row? x val)
                             (same-diagnol? x val)))
                       (cdr pair_list)))
       0)))

(length (queens 7))

; After the change, for every (enumerate-interval 1 board-size), it will calculate
; (queen-cols (- k 1) once. Also, every invoking on (queen-cols (- k 1)) will calculate
; board-size times of (queen-cols (- k 2). The total time is board-size^board-size of T
; when k is board-size. O(N^N) / N+1 = O(N^N). (8 ^ 8) * T = 823543 * T