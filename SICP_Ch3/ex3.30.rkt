#lang sicp
;; Section 3.3.2 Implementation of queue
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Section 3.3.4
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
        ((or (= a 0) (= b 0)) 0)
        (else (error "Invalid signal" a b))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 1)
        ((and (= a 1) (= b 0)) 1)
        ((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signal" a b))))

(define (make-wire)
  (let ((signal-value 0) (action-procedure '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedure))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedure (cons proc action-procedure))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknow"))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Exercise 3.30
;; The fowllowing code is from:
;; https://github.com/hjcapple/reading-sicp/blob/master/chapter_3/exercise_3_30.scm
(define (ripple-carry-adder a-list b-list s-list c-out)
  (define (helper-ripple-carry-adder a-list b-list s-list c-in c-out)
    (if (null? (cdr a-list))
        (full-adder (car a-list)
                    (car b-list)
                    c-in
                    (car s-list)
                    c-out)
        (let ((temp (make-wire)))
          (helper-ripple-carry-adder (cdr a-list);;the order is from right to left.
                                     (cdr b-list)
                                     (cdr s-list)
                                     c-in
                                     temp)
          (full-adder (car a-list)
                      (car b-list)
                      temp
                      (car s-list)
                      c-out)
          )))
  (let ((c-in (make-wire)))
    (set-signal! c-in 0)
    (helper-ripple-carry-adder a-list b-list s-list c-in c-out)
    'ok))

(define (make-wires n)
  (if (= n 0)
      '()
      (cons (make-wire) (make-wires (- n 1)))))

(define (set-wire-signals! wires values)
  (cond ((not (null? wires))
         (set-signal! (car wires) (car values))
         (set-wire-signals! (cdr wires) (cdr values)))))

(define (get-wire-signals wires)
  (if (null? wires)
      '()
      (append (list (get-signal (car wires))) 
              (get-wire-signals (cdr wires)))))

(define A (make-wires 4))
(define B (make-wires 4))
(define S (make-wires 4))
(define c-out (make-wire))
(ripple-carry-adder A B S c-out)

(define (print-info tag b)
  (display tag)
  (display b)
  (newline))

(define (run-simulate AB-values)
  (cond ((not (null? AB-values))
         (set-wire-signals! A (car (car AB-values)))
         (set-wire-signals! B (cadr (car AB-values)))
         (propagate)
         (print-info "" "==========")
         (print-info "time: " (current-time the-agenda))
         (print-info "a: " (get-wire-signals A))
         (print-info "b: " (get-wire-signals B))
         (print-info "s: " (get-wire-signals S))
         (print-info "c-out: " (get-signal c-out))
         (run-simulate (cdr AB-values)))))

(run-simulate '(
                ((0 0 0 0) (0 0 0 0))
                ((0 1 1 1) (0 1 1 1))
                ((1 1 1 1) (0 0 0 0))
                ((1 1 1 1) (0 0 0 1))
                ((1 1 1 1) (1 1 1 1))
                ))