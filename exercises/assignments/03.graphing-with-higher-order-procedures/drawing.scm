;;;; DRAWING.SCM
;;; Not for student reading -- uses continuation passing style for multiple
;;; value return

;;; Apply (DRAW-... WINDOW N) to a curve to compute N points on the curve
;;; and display those points with coordinates in WINDOW.

(define (draw-points-on window n)
  (let ((1/n (/ 1 n)))
    (lambda (curve)
      (define (iter count)
        (let ((t (* count 1/n)))
          (let ((ct (curve t)))
            (let ((x (x-of ct))
                  (y (y-of ct)))
              (graphics-draw-point
               window
               (exact->inexact x)
               (exact->inexact y))
              (if (>= count n)
                  'done
                  (iter (+ count 1)))))))
      (graphics-clear window)
      (iter 0))))

(define (draw-connected window n)
  (let ((1/n (/ 1 n)))
    (lambda (curve)
      (define (iter x-old y-old count)
        (let ((t (* count 1/n)))
          (let ((ct (curve t)))
            (let ((x-new (x-of ct))
                  (y-new (y-of ct)))
              (graphics-draw-line
               window
               x-old
               y-old
               (exact->inexact x-new)
               (exact->inexact y-new))
              (if (>= count n)
                  'done
                  (iter x-new y-new (+ count 1)))))))
      (graphics-clear window)
      (let ((c0 (curve 0)))
        (iter (x-of c0) (y-of c0) 1)))))


;;; Apply (DRAW-...-SQUEEZED-... WINDOW N) to a curve to squeeze it to exactly
;;; fit in WINDOW and then compute and display N points on the curve in WINDOW.

(define (draw-points-squeezed-to-window window n)
  (lambda (curve)    
    ((draw-points-on window n)
     (((corners curve n) squeeze-rectangular-portion) curve))))

(define (draw-connected-squeezed-to-window window n)
  (lambda (curve)    
    ((draw-connected window n)
     (((corners curve n) squeeze-rectangular-portion) curve))))

;;; CORNERS computes the max and min values of the x and y coordinates
;;; of n points on a given curve.
;;; It then applies a given procedure CORNERS-USER of type (NUM,NUM,NUM,NUM --> TYP)
;;; to the four corner values, where TYP may be any type.

;;; CORNERS is of type (CURVE,NUM) --> (((NUM,NUM,NUM,NUM) --> TYP) --> TYP), 

(define (corners curve n)
  (let ((1/n (/ 1 n)))
    (lambda (corners-user)
      ;;AUX calls receiver on corners of curve segment given by
      ;;parameter value t in the interval [1/count, 1]
      (define (aux count receiver)
        (let ((point (curve (* count 1/n))))
          (let ((xc (x-of point))
                (yc (y-of point)))
            (if (>= count n)
                (receiver xc xc yc yc)
                (aux (+ count 1)
                     (lambda (x- x+ y- y+)
                       (receiver
                        (min x- xc)
                        (max x+ xc)
                        (min y- yc)
                        (max y+ xc))))))))
      (aux 0 corners-user))))















