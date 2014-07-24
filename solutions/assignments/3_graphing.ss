#lang r5rs
(#%require racket/include)
(include "../lib/pic-lib.ss")
(include "../lib/arithmetic.ss")
(include "../lib/curves.scm")

;; Section 1
((compose log square) 2)

(define (thrice f)
  (compose (compose f f) f))

((thrice square) 2)

;; Exercise 1.A

;; yes we can pass thrice to thrice
;; (((thrice thrice) f) 0)
;; applied 3^3 = 27 times of inc to its argument
;; it equals to ((repeated f 27) 0)

(((thrice thrice) inc) 0)
((repeated inc 27) 0)

;; 1.
(((thrice thrice) inc) 6) ;; 33
;; 2.
(((thrice thrice) identity) compose) ;; the same as compose
;; 3.
(((thrice thrice) square) 1) ;; 1
;; 4.
;; (((thrice thrice) square) 2) ;; 2^(2^27) very large to print

;; Section 2

(#%require (only racket/math nan?))
(define make-point cons)
(define x-of car)
(define y-of cdr)
(define null-point (make-point +nan.0 +nan.0))
(define (null-point? point)
  (or
   ((compose not real?) (x-of point))
   ((compose not real?) (y-of point))
   (nan? (x-of point))
   (nan? (y-of point))))

(define (unit-circle t)
  (make-point (sin (* 2pi t))
              (cos (* 2pi t))))

(define (unit-line-at y)
  (lambda (t) (make-point t y)))

(define unit-line (unit-line-at 0))

(define (enumerate-unit-in step)
  (define (iter s e result)
    (if (> s e)
        result
        (iter (+ s step) e (cons s result))))
  (iter 0 1 '()))

;; Exercise 2
;; 1. What is the type of unit-line-at􏱨
;; Sch-Num -> Curve

;; 2. define vertical line
;; Point, Sch-Num -> Curve
(define (vertical-line point length)
  (let ((x (x-of point))
        (y (y-of point)))
    (lambda (t) (if (and (< (- t y) length) (positive? (- t y)))
                    (make-point x t) null-point))))

(paint-curve (vertical-line (make-point .5 .1) .3))

(define (rotate-pi/2 curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (y-of ct))
       (x-of ct)))))

(paint-curve (rotate-pi/2 unit-line))

;; Exercise 3
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (x-of ct))
       (y-of ct)))))

(define (reflect-through-x-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (x-of ct)
       (- (y-of ct))))))

(paint-curve (reflect-through-y-axis (vertical-line (make-point .5 .1) .2)))

;; some build-in curve transformer
(paint-curve ((rotate-around-origin 0.3)
              (vertical-line (make-point .5 .1) .2)))
(paint-curve ((scale-x-y .5 .3) unit-circle))

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

(define (square-curve t) (make-point t (square t)))
(define (cube-curve t) (make-point t (cube t)))
(define (linear-curve t) (make-point t t))

(paint-curve (put-in-standard-position square-curve))
(paint-curve (put-in-standard-position cube-curve))

(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t 1/2)
        (curve1 (* 2 t))
        (curve2 (- (* 2 t) 1)))))

(paint-curve (connect-rigidly ((scale .5) cube-curve)
                              ((translate .5 0) ((scale .5) (unit-line-at .5)))))

;; Exercise 4
(define (connect-ends curve1 curve2)
  (let* ((end-point-curve1 (curve1 1))
         (start-point-curve2 (curve2 0))
         (new-curve2 ((translate (- (x-of end-point-curve1)
                                    (x-of start-point-curve2))
                                 (- (y-of end-point-curve1)
                                    (y-of start-point-curve2)))
                      curve2)))
    (lambda (t)
      (if (< t 1/2)
          (curve1 (* 2 t))
          (new-curve2 (- (* 2 t) 1))))))

(paint-curve (connect-ends ((scale .5) cube-curve)
                           square-curve))

;; Section 3
(define (gosperize curve)
  (let ((scaled-curve ((scale (/ (sqrt 2) 2)) curve)))
    (connect-rigidly ((rotate-around-origin pi/4) scaled-curve)
                     ((translate .5 .5)
                      ((rotate-around-origin (- pi/4)) scaled-curve)))))

(define (gosper-curve level)
  ((repeated gosperize level) unit-line))

(define (show-connected-gosper level)
  (paint-curve (gosper-curve level) 1))

(show-connected-gosper 13)

;; for reference
;; http://www.inwap.com/pdp10/hbaker/hakmem/Figure8.html

;; Exercise 6.A

(define (show-points-gosper level curve)
  (paint-curve ((repeated gosperize level) curve)))

;; Exercise 6.B
(show-points-gosper 10 unit-circle)

;; now try to loose the angle being rotated
(define (param-gosper level angle-at origin-curve)
  (if (= level 0)
      origin-curve
      ((param-gosperize (angle-at level))
       (param-gosper (- level 1) angle-at origin-curve))))

(define (param-gosperize theta)
  (lambda (curve)
    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))
      (let ((scaled-curve ((scale scale-factor) curve)))
        (connect-rigidly ((rotate-around-origin theta) scaled-curve)
                         ((translate .5 (* (sin theta) scale-factor))
                          ((rotate-around-origin (- theta)) scaled-curve)))))))

(paint-curve (param-gosper 3 (lambda (l) (/ pi 6)) unit-line) 1.5)

;; Exercise 7.A
(define (param-gosperize theta)
  (lambda (curve)
    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))
      (let ((scaled-curve ((scale scale-factor) curve)))
        (put-in-standard-position
         (connect-ends ((rotate-around-origin theta) scaled-curve)
                       ((rotate-around-origin (- theta)) scaled-curve)))))))

(paint-curve (param-gosper 8 (lambda (l) pi/4) unit-line))

;; Exercise 7.B
(paint-curve (param-gosper 8 (lambda (l) (/ pi (+ l 2))) unit-line) 1.5)
(paint-curve (param-gosper 9 (lambda (l) (/ pi (expt 1.3 l))) unit-line) 2)

;; Exercise 7.C
(time (paint-curve (gosper-curve 10)))
;; cpu time: 3118 real time: 3119 gc time: 74
(time (paint-curve (param-gosper 10 (lambda (l) pi/4) unit-line)))
;; cpu time: 3494 real time: 3492 gc time: 488 (original version)
;; cpu time: 3044 real time: 3043 gc time: 30 (changed version)

;; There is no significant change to be noticed

;; Exercise 8.A/B
;; This definition is correct.
;; but the ben's version resulted in exponential recursion for `curve' procedure

;; for the original version, every rotate step will only consult the `curve' once
;; and the recursion is linear recursion

;; But the `rotate-around-origin' is called twice for each step, what it is linear?
;; A tricky point here is at the `connect-rigidly' procedure
(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t 1/2)
        (curve1 (* 2 t))
        (curve2 (- (* 2 t) 1)))))

;; it takes into procedures and apply them *for half the arguments* each
;; so as long as the two procedures have the same process shape
;; it will *remain the same shape*.
;; (connect-rigidly ... ) equals to one (rotate-around-origin ... ) procedure

(define (gosperize curve)
  (let ((scaled-curve ((scale (/ (sqrt 2) 2)) curve)))
    (connect-rigidly ((rotate-around-origin pi/4) scaled-curve)
                     ((translate .5 .5)
                      ((rotate-around-origin (- pi/4)) scaled-curve)))))

;; so the original call expands to a linear processes shape like
;; ->gosperize
;;   ->rotate-around-origin
;;     ->scale
;;         ...
;;         ->gosperize
;;           ->rotate-around-origin
;;             ->scale
;; so the time complexity is linear

;; with bens-rotate version, `rotate' will calculate `curve' twice, and result in a
;; tree recursion whose time is exponential

(for-each (lambda (level)
            (time (paint-curve (gosper-curve level))))
          '(1 2 3 4 5 6 7))

(begin (define _rotate-around-origin rotate-around-origin)
       (set! rotate-around-origin bens-rotate)
       (for-each (lambda (level)
                   (time (paint-curve (gosper-curve level))))
                 '(1 2 3 4 5 6 7))
       (set! rotate-around-origin _rotate-around-origin))

;; Exercise 9.A/B
;; start with some experiment
(paint-curve (param-gosper 5 (lambda (l)
                               (cond
                                ((= l 5) (/ pi 3))
                                ((= l 4) (/ pi 6))
                                ((= l 3) (- (/ pi 6)))
                                ((= l 2) (/ pi 6))
                                ((= l 1) (- (/ pi 6)))
                                )) unit-line) 2)

;; perform gosper transform for (/ pi 3) at the highest level
;; perform gosper transform (- pi/6) and pi/6 interveningly for each step

;; http://en.wikipedia.org/wiki/Koch_curve
(define (kochize level)
  (param-gosper 1 (lambda (l) (/ pi 3))
                (let ((angle (lambda (n)
                               (if (even? n) (/ pi 6) (- (/ pi 6))))))
                  (param-gosper (* 2 level) angle unit-line))))

(paint-curve (kochize 4) 1)

;; Exercise 10
(paint-curves (map (lambda (l)
       ((deriv-t 1) (gosper-curve l))) '(1 2 3 4 5 6 7 8 9)) 20)
