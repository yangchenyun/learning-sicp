#lang r5rs
(require racket/include)
(include "../lib/pic-lib.ss")

(define wave2 (beside wave (flip-vert wave)))
(paint wave2)

(define wave4 (below wave2 wave2))
(paint wave4)

;; now abstract the painter
(define (flipped-pairs painter)
  (let ((pair (beside painter (flip-vert painter))))
    (below pair pair)))

(paint (flipped-pairs wave))

;; painter could be defined recursively
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split wave 4))
(paint (right-split rogers 4))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split rogers 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (up (up-split painter (- n 1)))
            )
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (below
           (beside painter bottom-right)
           (beside top-left corner))))))

(paint (corner-split wave 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit wave 3))

;; we could abstract the painters which forms painters

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; redefine flipped-pairs and square-limit
(define (identity x) x)

(define flipped-pairs
  (square-of-four identity flip-vert
                  identity flip-vert))

(paint (flipped-pairs wave))

(define (square-limit painter n)
  (let ((combine (square-of-four flip-horiz identity
                                 rotate180 flip-vert)))
    (combine (corner-split painter n))))

(paint-hires (square-limit wave 3))

;; frame-* and vector-* procedures are provided by the painter.ss
(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v) (frame-edge1 frame))
                 (vector-scale (vector-ycor v) (frame-edge2 frame))))))

(vector-scale 3 (make-vect 1 2))

;; a painter is just a procedure to display something to the frame
;; it includes
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ;; this is a os-dependent procedure
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
segment-list)))

;; a transformer performs operations on the frame passed in
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))

;; flip over
(paint (transform-painter wave
                          (make-vect 0 1)
                          (make-vect 1 1)
                          (make-vect 0 0)))

;; shrink to the right corner
(paint (transform-painter wave
                          (make-vect .5 .5)
                          (make-vect 1 .5)
                          (make-vect .5 1)))

;; rotate 90
(paint (transform-painter wave
                          (make-vect 0 1)
                          (make-vect 0 0)
                          (make-vect 1 1)))

;; squash inwards
(paint (transform-painter wave
                          (make-vect 0 0)
                          (make-vect .65 .35)
                          (make-vect .35 .65)))

;; compound painter is also defined by frame transformation
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter
                       painter1
                       (make-vect 0 0)
                       split-point
                       (make-vect 0 1)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1 0)
                        (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
