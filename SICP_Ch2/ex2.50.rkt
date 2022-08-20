#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

(define wave einstein)
;(paint wave)

; Transforming and combining painters
(define sub-vect1 vector-sub)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect1 (m corner1) new-origin)
                     (sub-vect1 (m corner2) new-origin)))))))

; Exercise 2.50
(define (flip-horiz-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)    ; new origin
                     (make-vect 0.0 0.0)    ; new end of edge1
                     (make-vect 1.0 1.0)))  ; new end of edge2
(paint (flip-horiz-v1 wave))

(define (rotate180-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(paint (rotate180-v1 wave))

(define (rotate270-v1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(paint (rotate270-v1 wave))
