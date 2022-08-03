#lang sicp
; Exercise 2.42

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

; We can guess that one position is denoted as row, since
; columns are already in order without concern of same col.
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
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

(queens 5)

(length (queens 8))