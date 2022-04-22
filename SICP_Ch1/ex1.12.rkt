#lang racket

;Exercise 1.12
(define (pascal row)
  (define (construct_pascal row col)
    (cond ((= row 1) (list 1))
          ((= row 2) (list 1 1))
          (else (cond ((= col (+ row 1)) '())
                      (else (cons (pascal_ele row col)
                                  (construct_pascal row (+ col 1))))))))
  (define (pascal_ele row col)
    (cond ((or (= col 1) (= row 1)) 1)
          ((= col row) 1)
          (else (+ (pascal_ele (- row 1) (- col 1))
                   (pascal_ele (- row 1) col)))))
  (construct_pascal row 1))
; test case from: https://github.com/hjcapple/reading-sicp/blob/master/chapter_1/exercise_1_12.scm
(define (for-loop n last op)
  (cond ((<= n last)
         (op n)
         (for-loop (+ n 1) last op))))

(define (print-pascal-triangle n)
  (define (print-pascal-row input)
    (displayln (pascal input)))
  (for-loop 1 n print-pascal-row))

(print-pascal-triangle 13)