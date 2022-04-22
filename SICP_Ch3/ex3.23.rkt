#lang sicp
;; Pictures refer to: 
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/exercise_3_23.md
(define (make-deque)
  (cons '() '()))

(define (make-deque-node item prev next)
  (cons item (cons prev next)))

(define (set-node-prev! node prev)
  (set-car! (cdr node) prev))

(define (set-node-next! node next)
  (set-cdr! (cdr node) next))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (set-front-ptr! deque node)
  (set-car! deque node))

(define (set-rear-ptr! deque node)
  (set-cdr! deque node))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty queue" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-node (make-deque-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-node-prev! (front-ptr deque) new-node)
           (set-node-next! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-deque-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-node-next! (rear-ptr deque) new-node)
           (set-node-prev! new-node (rear-ptr deque))
           (set-rear-ptr! deque new-node)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE on empty deque" deque))
        ((eq? (front-ptr deque)
              (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (set-node-prev! (front-ptr deque) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE on empty deque" deque))
        ((eq? (front-ptr deque)
              (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (set-node-next! (rear-ptr deque) '())
         deque)))

(define (print-deque deque)
  (define (print-node node)
    (cond ((not (null? node))
           (display (car node))
           (cond ((not (null? (cdr (cdr node))))
                  (display " ")))
           (print-node (cdr (cdr node))))))
  (display "(")
  (print-node (front-ptr deque))
  (display ")")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define q (make-deque))
(print-deque q)

(front-insert-deque! q 'a)
(print-deque q)

(front-insert-deque! q 'b)
(print-deque q)

(rear-insert-deque! q 'b)
(print-deque q)

(rear-insert-deque! q 'c)
(print-deque q)

(front-delete-deque! q)
(print-deque q)

(front-delete-deque! q)
(print-deque q)

(rear-delete-deque! q)
(print-deque q)

(rear-delete-deque! q)
(print-deque q)