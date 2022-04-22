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
  (define (dispatch m) ;; Slight change here to get different number of arguments
    (lambda (proc)
      (if (equal? m password)
          (cond ((eq? proc 'withdraw) withdraw)
                ((eq? proc 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             proc)))
          (lambda (x) "Incorrect password"))))
    dispatch)

(define peter-acc (make-account 100 'open-sesame))

(define (make-joint acc acc-password acc-joint-password)
  (if (= (((acc acc-password) 'withdraw) 0) ;; check if password is right, for free
         (((acc acc-password) 'deposit) 0))
      (lambda (joint-acc)
        (if (eq? joint-acc acc-joint-password)
            (acc acc-password)
            (error "Wrong password!" joint-acc)))
      (error "Denial to joint account due to wrong password!" acc-password)))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(((peter-acc 'open-sesame) 'withdraw) 40)
(((paul-acc 'rosebud) 'deposit) 50)
(((peter-acc 'open-sesame) 'withdraw) 70)
; (((paul-acc 'open-sesame) 'deposit) 50)
