#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m proc)
    (if (equal? m password)
        (cond ((eq? proc 'withdraw) withdraw)
              ((eq? proc 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           proc)))
        (lambda (x) "Incorrect password"))) ;; Attention! It must return a function here, not string.
  dispatch)                                 ;; Still need to check balance in the future.

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'secret-password 'withdraw) 70)

((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)