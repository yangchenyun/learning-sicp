#lang r5rs
(require (planet soegaard/sicp:2:1/sicp))
;; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html

;; the rogers painter
(define rogers
  (load-painter
   (build-path ".." "rogers.jpg")))

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

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split rogers 4))

;; Exercise 2.45
(define (split origin-split-combination split-combination)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let* ((combine (split origin-split-combination split-combination))
               (smaller (combine painter (- n 1))))
          (origin-split-combination painter (split-combination smaller smaller))))))

(define right-split (split beside below))
(paint (right-split rogers 4))

(define up-split (split below beside))
(paint (up-split rogers 4))

;; Exercise 2.46
;; an implementation of vector
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car x))
(define (ycor-vect v) (cadr y))

(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect
   (- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect v n)
  (make-vect
   (* (xcor-vect v) n)
   (* (ycor-vect v) n)))

;; Exercise 2.47
;; for implementation 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (caddr frame))

(frame-origin (make-frame 1 2 3))
(frame-edge1 (make-frame 1 2 3))
(frame-edge2 (make-frame 1 2 3))

;; for implementation 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (cddr frame))

(frame-origin (make-frame 1 2 3))
(frame-edge1 (make-frame 1 2 3))
(frame-edge2 (make-frame 1 2 3))

;; Exercise 2.48
(define make-segment cons)
(define segment-start car)
(define segment-end cadr)

;; Exercise 2.49
;; a.
(define outline-segments
  (let ((tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (bl (make-vect 0 0))
        (br (make-vect 1 0)))
    (list (make-segment bl tl)
          (make-segment tl tr)
          (make-segment tr br)
          (make-segment br bl))))

(paint (segments->painter outline-segments))

;; b.
(define x-segments
  (let ((tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (bl (make-vect 0 0))
        (br (make-vect 1 0)))
    (list (make-segment bl tr)
          (make-segment tl br))))

(paint (segments->painter x-segments))

;; c.
(define diamond-segments
  (let ((top-mid (make-vect .5 1))
        (bottom-mid (make-vect .5 0))
        (left-mid (make-vect 0 .5))
        (right-mid (make-vect 1 .5)))
    (list (make-segment top-mid left-mid)
          (make-segment top-mid right-mid)
          (make-segment bottom-mid left-mid)
          (make-segment bottom-mid right-mid))))

(paint (segments->painter diamond-segments))

;; d.
;; defined before

;; Exercise 2.50
;; a transformer performs operations on the frame passed in
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint (flip-horiz einstein))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(paint (rotate180 einstein))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint (rotate270 einstein))

;; Exercise 2.51
(define (below bottom-painter top-painter)
  (let ((split-point (make-vect 0 .5)))
    (let ((up-painter (transform-painter
                       top-painter
                       split-point
                       (make-vect 1 .5)
                       (make-vect 0 1)))
          (bottom-painter (transform-painter
                           bottom-painter
                           (make-vect 0 0)
                           (make-vect 1 0)
                           split-point)))
      (lambda (frame)
        (up-painter frame)
        (bottom-painter frame)))))

(paint (below einstein rogers))

(define (below bottom-painter top-painter)
  (lambda (frame)
    ((rotate90 (beside (rotate270 bottom-painter)
                       (rotate270 top-painter))) frame)))

(paint (below einstein rogers))

;; Exercise 2.52
;; a.
(define smile-wave-segments
  (append (list (make-segment
          (make-vect 0.433 0.8)
          (make-vect 0.5 0.75))
         (make-segment
          (make-vect 0.567 0.8)
          (make-vect 0.5 0.75))
         (make-segment
          (make-vect 0.567 0.9)
          (make-vect 0.56 0.9))
         (make-segment
          (make-vect 0.433 0.9)
          (make-vect 0.450 0.9)))
          wave-segments))

(paint (segments->painter smile-wave-segments))

;; b.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (up (up-split painter (- n 1)))
            (corner (corner-split painter (- n 1)))
            )
        (below
         (beside painter right)
         (beside up corner)))))

(paint (corner-split wave 4))

;; c.
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (rotate270 quarter) (rotate90 (flip-horiz quarter)))))
      (below (flip-vert half) half))))

(paint (square-limit einstein 3))