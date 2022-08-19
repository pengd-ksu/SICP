#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

; Exercise 2.46
; make-frame-v1 is only a practice, there's a real implementation for drawing purpose,
; as show in transform-painter below
(define (make-frame-v1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v0 v1)
  (make-vect (+ (xcor-vect v0) (xcor-vect v1))
             (+ (ycor-vect v0) (ycor-vect v1))))

(define (sub-vect v0 v1)
  (make-vect (- (xcor-vect v0) (xcor-vect v1))
             (- (ycor-vect v0) (ycor-vect v1))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (frame-coord-map-v1 frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define a-frame (make-frame-v1 (make-vect 0 0)
                               (make-vect 2 0)
                               (make-vect 0 2)))
((frame-coord-map-v1 a-frame) (make-vect 0 0))
((frame-coord-map-v1 a-frame) (make-vect 1 1))
(origin-frame a-frame)