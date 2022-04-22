#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (define (mf input)
      (cond ((eq? input 'how-many-calls?)
             count)
            ((eq? input 'reset-count)
             (set! count 0))
            (else (begin (set! count (+ count 1))
                         (f input)))))
    mf))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(s 36)

(s 81)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)
