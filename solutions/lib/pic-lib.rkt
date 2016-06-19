#lang racket

(require (only-in 2htdp/image
                  rectangle
                  add-line
                  circle
                  empty-scene
                  place-image
                  underlay
                  overlay))

(require (only-in racket/base foldr))
(require "./utils.rkt")
(require "./curves.rkt")

(provide (all-defined-out))

;; the vector
(define make-vect cons)
(define vector-xcor car)
(define vector-ycor cdr)

(define (vector-add v1 v2)
  (make-vect (+ (vector-xcor v1) (vector-xcor v2))
             (+ (vector-ycor v1) (vector-ycor v2))))

(define (vector-sub v1 v2)
  (vector-add v1 (vector-scale -1 v2)))

(define (vector-scale x v)
  (make-vect (* x (vector-xcor v))
             (* x (vector-ycor v))))

;; the segment
(define make-segment cons)
(define segment-start car)
(define segment-end cdr)

;; the point

;; the frame
(define (make-frame origin edge1 edge2)
  (list 'frame origin edge1 edge2))

(define frame-origin cadr)
(define frame-edge1 caddr)
(define frame-edge2 cadddr)

;; shift and scale vectors to fit the frame
(define (frame-coord-map frame)
  (lambda (point-in-frame-coords)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor point-in-frame-coords)
			       (frame-edge1 frame))
		 (vector-scale (vector-ycor point-in-frame-coords)
                               (frame-edge2 frame))))))

(define *screen-width* 128)
(define *screen-height* 128)
(define *last-screen-row* (- *screen-height* 1))
(define *last-screen-column* (- *screen-width* 1))
(define *canvas* (empty-scene *screen-width* *screen-height* "transparent"))
(define *screen* (rectangle *screen-width* *screen-height* "solid" "white"))

(define (set-painter-resolution! res)
  (let ((res (inexact->exact res)))
    (set! *screen-width* res)
    (set! *screen-height* res)
    (set! *last-screen-row* (- *screen-height* 1))
    (set! *last-screen-column* (- *screen-width* 1))
    (set! *screen* (rectangle *screen-width* *screen-height* "solid" "white"))
    (set! *canvas* (empty-scene *screen-width* *screen-height* "transparent"))
    (set! screen-frame
          ((make-relative-frame (make-vect 0 *screen-height*)
                                (make-vect *screen-width* *screen-height*)
                                (make-vect 0 0)) unit-square-frame))
    'set))

;; a transformer performs operations on the frame passed in
(define (make-relative-frame origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(make-frame new-origin
		    (vector-sub (m corner1) new-origin)
		    (vector-sub (m corner2) new-origin))))))

(define unit-square-frame (make-frame (make-vect 0 0)
                                      (make-vect 1 0)
                                      (make-vect 0 1)))

;; the racket draw system use top-left as origin point
(define screen-frame
  ((make-relative-frame (make-vect 0 *screen-height*)
                        (make-vect *screen-width* *screen-height*)
                        (make-vect 0 0)) unit-square-frame))

;; primitive draw operations
(define (draw-line image s e)
  (let ((sx (vector-xcor s))
        (sy (vector-ycor s))
        (ex (vector-xcor e))
        (ey (vector-ycor e)))
    (add-line image sx sy ex ey "black")))

(define (draw-point image p)
  (let ((point (circle .3 "solid" "black"))
        (px (vector-xcor p))
        (py (vector-ycor p)))
    (place-image point px py image)))

;; segments painter
(define (segments->painter segment-list)
  (lambda (frame)
    (foldr
     (lambda (segment canvas)
       (draw-line canvas
                  ((frame-coord-map frame)
                   (segment-start segment))
                  ((frame-coord-map frame)
                   (segment-end segment))))
     *canvas*
     segment-list)))

(define (points->painter point-list)
  (lambda (frame)
    (foldr
     (lambda (point canvas)
       (draw-point canvas
                  ((frame-coord-map frame)
                   (point->vector point)))) ;; treat point as vector
     *canvas*
     (filter (compose not null-point?) point-list))))

;; painter takes a frame and paint
(define (paint painter)
  (underlay *screen* (painter screen-frame)))

(define (paint-hires painter)
  (set-painter-resolution! 512)
  (underlay *screen* (painter screen-frame)))

;; only plot the [0 1] region
(define (enumerate-unit-in step)
  (define (iter s e result)
    (if (> s e)
        result
        (iter (+ s step) e (cons s result))))
  (iter 0 1 '()))

(define (paint-curves curves . canvas-scale)
  (let* ((canvas-scale (if (null? canvas-scale) 1 (car canvas-scale)))
         (map-scale (/ .5 canvas-scale))
         (transformer (transform-painter
                       (make-point .5 .5)
                       (make-point (+ map-scale .5) .5)
                       (make-point .5 (+ map-scale .5)))))
    (paint (transformer
            (points->painter (foldr append '()
                                    (map (lambda (curve)
                                           (map curve (enumerate-unit-in 0.0001)))
                                         curves)))))))

(define (paint-curve curve . canvas-scale)
  (if (null? canvas-scale)
      (paint-curves (list curve))
      (paint-curves (list curve) (car canvas-scale))))

;; a procedure which applies the transformed frame on the painter
(define (transform-painter origin corner1 corner2)
  (lambda (painter)
    (compose painter
             (make-relative-frame
              origin
              corner1
              corner2))))

;;;; Basic means of combination for painters
(define flip-horiz
  (transform-painter (make-vect 1 0)
		     (make-vect 0 0)
		     (make-vect 1 1)))

(define flip-vert
  (transform-painter (make-vect 0 1)
		     (make-vect 1 1)
		     (make-vect 0 0)))

(define rotate90
  (transform-painter (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define rotate180 (repeated rotate90 2))
(define rotate270 (repeated rotate90 3))

(define (superpose painter1 painter2)
  (lambda (frame)
    (overlay (painter1 frame)
             (painter2 frame))))

;; painter takes a frame and paint
(define (beside painter1 painter2)
  (let ((split-point (make-vect .5 0)))
    (superpose
     ((transform-painter (make-vect 0 0)
			 split-point
			 (make-vect 0 1))
      painter1)
     ((transform-painter split-point
			 (make-vect 1 0)
			 (make-vect .5 1))
      painter2))))

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

;; The wave painter
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))