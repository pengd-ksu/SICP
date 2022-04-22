#lang sicp
(define (rand-update x)
  (let ((a 47843) (b 61091) (m 44971))
    (modulo (+ (* a x) b) m)))

(define random-init 11)

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unkonwn operation" m))))))

((rand 'reset) 1000)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1000)
(rand 'generate)
(rand 'generate)