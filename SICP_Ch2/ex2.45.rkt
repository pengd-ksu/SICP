#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

(define wave einstein)
;(paint wave)
;(paint (below wave (flip-vert wave))); we see first arg is below
(define wave2 (beside wave (flip-vert wave)))
;(paint wave2)
;(define wave4 (below wave2 wave2))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4 (flipped-pairs wave))
;(paint wave4)

(define (right-split-v1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split-v1 painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split-v1 wave 8))

; Exercise 2.44
(define (up-split-v1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-v1 painter (- n 1))))
        (below painter
               (beside smaller
                       smaller)))))
;(paint (up-split-v1 wave 4))

; Exercise 2.45
(define (split o1 o2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split o1 o2) painter (- n 1))))
          (o1 painter
              (o2 smaller
                  smaller))))))

(define right-split-v2 (split beside below))

(define up-split-v2 (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split-v2 painter (- n 1)))
            (right (right-split-v2 painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(paint (corner-split wave 4))
