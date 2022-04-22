#lang sicp

(define (cont-frac-recur n d k)
  (define (aux i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (aux (+ i 1))))))
  (aux 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (/ (n (- i 1))
                         (+ (d (- i 1))
                            result)))))
  (iter k (/ (n k)
             (d k))))

(define (naturalBase k)
  (define (d-fn i)
    (if (or (= (remainder i 3) 0)
            (= (remainder i 3) 1))
        1
        (/ (* (+ i 1) 2)
           3)))
  (+ (cont-frac-recur (lambda (i) 1.0)
                      d-fn
                      k)
     2))

(naturalBase 10000)