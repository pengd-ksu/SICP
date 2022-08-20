#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

(define wave einstein)
;(paint wave)

(define (rotate90-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
;(paint (rotate90-v1 wave))

(define (beside-v1 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
;(paint (beside-v1 wave (flip-vert-v1 wave)))

(define (rotate270-v1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
; (paint (rotate270-v1 wave))

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


(define (flip-vert-v1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2
;(paint (flip-vert-v1 wave))

; Exercise 2.51
(define (below-v1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
(paint (below-v1 wave (flip-vert-v1 wave)))
(define (below-v2 painter1 painter2)
  (rotate90-v1 (beside-v1 (rotate270-v1 painter1) (rotate270-v1 painter2))))
(paint (below-v2 wave (flip-vert-v1 wave)))