;;;; CURVES.SCM

                                        ;Point = (Sch-Num X Sch-Num)

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))
                                        ;Unit-Interval = {x: Sch-Num | 0 <= x <= 1}
                                        ;Curve = Unit-interval --> Point

                               ;;SOME CURVES

(define (unit-circle t)
  (make-point (sin (* 2pi t))
              (cos (* 2pi t))))

(define (unit-line t)
  (make-point t 0))

(define (alternative-unit-circle t)
  (make-point (sin (* 2pi (square t)))
              (cos (* 2pi (square t)))))

                                        ;Curve-Transform = (Curve --> Curve)

                              ;;SOME CURVE-TRANSFORMS


(define (rotate-pi/2 curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (y-of ct))
       (x-of ct)))))

                           ;;CONSTRUCTORS OF CURVE-TRANSFORMS

;;; TRANSLATE is of type (Sch-Num, Sch-Num --> Curve-Transform)

(define (translate x0 y0)               
  (lambda (curve)
    (lambda (t)
      (let ((ct (curve t)))
        (make-point (+ x0 (x-of ct))
                    (+ y0 (y-of ct)))))))

;;; ROTATE-AROUND-ORIGIN is of type (Sch-Num --> Curve-Transform)

(define (rotate-around-origin theta)
  (let ((cth (cos theta))
        (sth (sin theta)))
    (lambda (curve)
      (lambda (t)
        (let ((ct (curve t)))
          (let ((x (x-of ct))
                (y (y-of ct)))
            (make-point
             (- (* cth x) (* sth y))
             (+ (* sth x) (* cth y)))))))))

(define (bens-rotate theta) ;rotates around origin, but less efficiently
  (let ((cth (cos theta))
        (sth (sin theta)))
    (lambda (curve)
      (lambda (t)
        (let ((x (x-of (curve t)))
              (y (y-of (curve t))))
          (make-point
           (- (* cth x) (* sth y))
           (+ (* sth x) (* cth y))))))))

(define (deriv-t n)
  (let ((delta_t (/ 1 n)))
    (lambda (curve)
      (lambda (t)
        (let ((ct (curve t))
              (ctdelta (curve (+ t delta_t))))
          (make-point (/ (- (x-of ctdelta) (x-of ct))
                         delta_t)
                      (/ (- (y-of ctdelta) (y-of ct))
                         delta_t)))))))

(define (scale-x-y a b)
  (lambda (curve)
    (lambda (t)
      (let ((ct (curve t)))
        (make-point (* a (x-of ct))
                    (* b (y-of ct)))))))

(define (scale s)
  (scale-x-y s s))

;;; SQUEEZE-RECTANGULAR-PORTION translates and scales a curve
;;; so the portion of the curve in the rectangle
;;; with corners xlo xhi ylo yhi will appear in a display window
;;; which has x, y coordinates from 0 to 1.
;;; It is of type (Sch-Num,Sch-Num,Sch-Num,Sch-Num --> Curve-Transform).

(define (squeeze-rectangular-portion xlo xhi ylo yhi)
  (compose (scale-x-y (/ 1 (- xhi xlo))
                      (/ 1 (- yhi ylo)))
           (translate (- xlo) (- ylo))))


;;; PUT-IN-STANDARD-POSITION is a Curve-Transform.
;;; A curve is in "standard position" if it starts at (0,0) ends at (1,0).
;;; A curve is PUT-IN-STANDARD-POSITION by rigidly translating it so its
;;; start point is at the origin, then rotating it about the origin to put
;;; its endpoint on the x axis, then scaling it to put the endpoint at (1,0).

(define (put-in-standard-position curve)
  (let* ((start-point (curve 0))
         (curve-started-at-origin
          ((translate (- (x-of start-point))
                      (- (y-of start-point)))
           curve))
         (new-end-point (curve-started-at-origin 1))
         (theta (atan (y-of new-end-point) (x-of new-end-point)))
         (curve-ended-at-x-axis
          ((rotate-around-origin (- theta)) curve-started-at-origin))
         (end-point-on-x-axis (x-of (curve-ended-at-x-axis 1))))
    ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis)))


                                        ;Binary-transform = (Curve,Curve --> Curve)

;;; CONNECT-RIGIDLY makes a curve consisting of curve1 followed by curve2.

(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t (/ 1 2))
	(curve1 (* 2 t))
	(curve2 (- (* 2 t) 1)))))

;;; CONNECT-ENDS makes a curve consisting of curve1 followed by
;;;  a copy of curve2 starting at the end of curve1

;;;(define (connect-ends curve1 curve2) ...)

                          ;;FRACTAL CURVES

;;; GOSPERIZE is a Curve-Transform

(define (gosperize curve)
  (let ((scaled-curve ((scale (/ (sqrt 2) 2)) curve)))
    (connect-rigidly ((rotate-around-origin (/ pi 4)) scaled-curve)
                     ((translate .5 .5)
                      ((rotate-around-origin (/ -pi 4)) scaled-curve)))))


;;; GOSPER-CURVE is of type (Sch-Num --> Curve)

(define (gosper-curve level)
  ((repeated gosperize level) unit-line))


                      ;;DRAWING GOSPER CURVES

(define (show-connected-gosper level)
  ((draw-connected g1 200)
   ((squeeze-rectangular-portion -.5 1.5 -.5 1.5)
    (gosper-curve level))))


                      ;;PARAMETERIZED GOSPER

;;; PARAM-GOSPER is of type ((Sch-Num,(Int --> Sch-Num)) --> Curve)

(define (param-gosper level angle-at)
  (if (= level 0)
      unit-line
      ((param-gosperize (angle-at level))
       (param-gosper (- level 1) angle-at))))

(define (param-gosperize theta)
  (lambda (curve)
    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))
      (let ((scaled-curve ((scale scale-factor) curve)))
        (connect-rigidly ((rotate-around-origin theta) scaled-curve)
                         ((translate .5 (* (sin theta) scale-factor))
                          ((rotate-around-origin (- theta)) scaled-curve)))))))





















