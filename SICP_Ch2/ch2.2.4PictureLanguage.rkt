#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; use implementation provided by racket
; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
; use einstein as the basic painter

(define wave einstein)
;(paint wave)
(define wave2 (beside wave (flip-vert wave)))
;(paint wave2)
;(define wave4 (below wave2 wave2))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4 (flipped-pairs wave))
;(paint wave4)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split wave 4))

; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;(paint (up-split wave 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;(paint (corner-split wave 4))

(define (square-limit-v1 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;(paint (square-limit-v1 wave 4))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;(paint ((square-of-four identity flip-vert identity flip-horiz) wave))

(define (flipped-pairs-2 painter)
  (let ((combine4 (square-of-four identity flip-vert 
                                  identity flip-vert)))
    (combine4 painter)))
;(paint (flipped-pairs-2 wave))

(define (square-limit-v2 painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
(paint (square-limit-v1 wave 3))
(paint (square-limit-v2 wave 3))

; Exercise 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))
(define right-split1 (split beside below))
(define up-split1 (split below beside))
; (paint (right-split1 wave 2))
; (paint (up-split wave 2))

; Frame
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
; ((frame-coord-map-v1 a-frame) (make-vect 0 0)) -> '(0 . 0)
; ((frame-coord-map-v1 a-frame) (make-vect 1 1)) -> '(2 . 2)
; (origin-frame a-frame) -> '(0 . 0)

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

; Painter
; The segments->painter-practice and draw-line-practice are hypothetical examples
; There's real implementation of segments->painter provided by sicp.plt
(define (segments->painter-practice segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line-practice
        ((frame-coord-map-v1 frame) (start-segment segment))
        ((frame-coord-map-v1 frame) (end-segment segment))))
     segment-list)))

(define (draw-line-practice v1 v2)
  (newline)
  (display v1)
  (display "->")
  (display v2))

; Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; Exercise 2.49
; 1 would be out of range, 0.99 would ensure every line segments are visible
(define designated-frame
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 0 0.99))
                      (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99))
                      (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0))
                      (make-segment (make-vect 0.99 0) (make-vect 0 0)))))
; (paint designated-frame)

(define x-frame
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 1))
                      (make-segment (make-vect 0 1) (make-vect 1 0)))))
; (paint x-frame)

(define diamond-frame
  (segments->painter (list
                      (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                      (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                      (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))))
; (paint diamond-frame)

(define George
  (segments->painter (list
                      (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                      (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                      (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                      (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                      (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                      (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                      (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                      (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                      (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                      (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                      (make-segment (make-vect .4 1) (make-vect .6 1)) 
                      (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                      (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                      (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                      (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                      (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                      (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                      (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                      (make-segment (make-vect .75 0) (make-vect .6 0)) 
                      (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                      (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                      (make-segment (make-vect .4 0) (make-vect .25 0)))))
; (paint George)

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
; (paint (flip-vert-v1 wave))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
; (paint (shrink-to-upper-right wave))

(define (rotate90-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
; (paint (rotate90-v1 wave))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
; (paint (squash-inwards wave)); error, doesn't support it.

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
; (paint (beside-v1 wave (flip-vert-v1 wave)))

; Exercise 2.50
(define (flip-horiz-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)    ; new origin
                     (make-vect 0.0 0.0)    ; new end of edge1
                     (make-vect 1.0 1.0)))  ; new end of edge2
; (paint (flip-horiz-v1 wave))

(define (rotate180-v1 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 .0)))
; (paint (rotate180-v1 wave))

(define (rotate270-v1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
; (paint (rotate270-v1 wave))

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
; (paint (below-v1 wave (flip-vert-v1 wave)))
(define (below-v2 painter1 painter2)
  (rotate90-v1 (beside-v1 (rotate270-v1 painter1) (rotate270-v1 painter2))))
; (paint (below-v2 wave (flip-vert-v1 wave)))