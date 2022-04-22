#lang sicp

(define (make-account balance password)
  (define (call-the-cops)
      (lambda (x)
        "Cops on the way!"))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-pass-count 0))
    (define (dispatch m proc)
      (if (equal? m password)
          (begin (set! wrong-pass-count 0)
                 (cond ((eq? proc 'withdraw) withdraw)
                       ((eq? proc 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    proc))))
          (begin
            (set! wrong-pass-count (+ wrong-pass-count 1))
            (if (>= wrong-pass-count 7)
                (call-the-cops)
                (lambda (x) "Incorrect password")))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'secret-password 'withdraw) 70)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)