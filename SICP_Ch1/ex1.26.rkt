#lang sicp

; Exercise 1.26
; The original expmod will invoke (expmod base (/ exp 2) m) once when exp is even, therefore
; the complexity is O(lgn). But Reasoner's implementation will call
; (expmod_Reasoner base (/ exp 2) m) twice when exp is even. So the reduction of complexity
; will be concelled by the double invoking. So Reasoner's method still has O(n) complexity.
(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod_Reasoner base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod_Reasoner base (/ exp 2) m)
                       (expmod_Reasoner base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod_Reasoner base (- exp 1) m))
                    m))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))