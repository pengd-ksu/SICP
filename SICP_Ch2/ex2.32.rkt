#lang sicp
; Exercise 2.32
; Recursive way of thinking, when you have a subsets s1 (a list) from the whole set
; except the first one (e1), you can append s1 and (cons e1 s1) to get all the
; subsets. And don't forget the base condition: when the set is null.
; The other way of thinking is, we have two subsets of s, one containing (car s),
; the other doesn't. Combine them together, we will get all the subsets. Very similar
; to counting change mentioned in the book previously.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(subsets (list 1 2 3))