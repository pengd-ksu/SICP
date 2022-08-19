#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

; Exercise 2.47
(define (make-frame-v2 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame-v2 frame)
  (car frame))
(define (edge1-frame-v2 frame)
  (cadr frame))
(define (edge2-frame-v2 frame)
  (caddr frame))

(define (make-frame-v3 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-v3 frame)
  (car frame))
(define (edge1-frame-v3 frame)
  (cadr frame))
(define (edge2-frame-v3 frame)
  (cddr frame))
