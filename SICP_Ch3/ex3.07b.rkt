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
        (lambda (x) "Incorrect password")))
  dispatch)

;; The following code is from
;; https://eli.thegreenplace.net/2007/09/27/sicp-sections-312-313
;; This one doesn't need to change answer from Exercise 3.3
(define (make-joint acc acc-pass new-pass)
  (define (proxy-dispatch password m)
    (if (eq? password new-pass)
      (acc acc-pass m) ;; Will take care of wrong code when joining account
      (error "Bad joint password -- " password)))
  proxy-dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'deposit) 50)
((peter-acc 'open-sesame 'withdraw) 70)
;; ((paul-acc 'open-sesame 'deposit) 50)