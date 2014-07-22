#lang r5rs

(define (numer r) (car r))
(define (denom r) (cdr r))
(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))

(define tolerence 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? u v)
    (< (abs (- u v)) tolerence))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 2.1
;; improved version to handle both positive and negative arguments
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (positive? (* n d)) + -)))
    (cons (sign (abs (/ n g))) (abs (/ d g)))))

(print-rat (make-rat -1 -2))
(print-rat (make-rat 1 -2))

;; Exercise 2.2
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; pointer constructor and selector
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; segment constructor and selector
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (let ((sp (start-segment segment))
        (ep (end-segment segment)))
    (make-point (average (x-point sp) (x-point ep))
                (average (y-point sp) (y-point ep)))))

(define sp (make-point -5 10))
(define ep (make-point 3 15))
(midpoint-segment (make-segment sp ep))

;; Exercise 2.3

(define (segment-length segment)
  (let ((sp (start-segment segment))
        (ep (end-segment segment)))
    (sqrt (+ (square (- (x-point sp) (x-point ep)))
             (square (- (y-point sp) (y-point ep)))))))

(segment-length (make-segment
                (make-point 5 10)
                (make-point 8 14)))

;; represent the rectangles with two adjacent segments
;; should validate the w-seg and h-seg is 90' and intersected at start point
(define (make-rect w-seg h-seg) (cons w-seg h-seg))
(define (rect-width rect) (segment-length (car rect)))
(define (rect-height rect) (segment-length (cdr rect)))

(define (perimeter rect)
  (* 2 (+ (rect-width rect)
          (rect-height rect))))

(define (area rect)
  (* (rect-width rect) (rect-height rect)))

(define rect1 (make-rect
               (make-segment
                (make-point 0 0)
                (make-point 0 10))
               (make-segment
                (make-point 0 0)
                (make-point 5 0))))

(perimeter rect1)
(area rect1)

;; now represent the rect with three points
;; p1 +
;;    |
;;    |
;; p2 +----+ p3

(define (make-rect p1 p2 p3)
  (cons (make-segment p1 p2)
        (make-segment p2 p3)))
(define (rect-width rect) (segment-length (car rect)))
(define (rect-height rect) (segment-length (cdr rect)))

(define rect2 (make-rect
                (make-point 5 0)
                (make-point 0 0)
                (make-point 0 10)))

(perimeter rect2)
(area rect2)

;; Exercise 2.4
;; (car (cons x y))
;; use the substitution model
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; y

(define (cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (find-power number root)
  (define (iter number power)
    (if (= 0 (modulo number root))
        (iter (/ number root) (+ 1 power))
        power))
  (iter number 0))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car z) (find-power z 2))
(define (cdr z) (find-power z 3))

(car (cons 3 8))
(cdr (cons 3 8))

;; Exercise 2.6
;; church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x))) ;; this is one

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; make a guess
(define (add-2 n)
  (lambda (f) (lambda (x) (f (f ((n f) x))))))

(add-2 one)
(add-2 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (f (((lambda (f) (lambda (x) (f x))) f) x)))))
(lambda (f) (lambda (x) (f (f (((lambda (f) (lambda (x) (f x))) f) x)))))
(lambda (f) (lambda (x) (f (f ((lambda (x) (f x)) x)))))
(lambda (f) (lambda (x) (f (f (f x))))) ;; 3

;; now make another guess for `add'
;; an anonymous procedure which takes a procedure f
;; and return a procedure with one argument x and apply
;; m times f to ((n f) x)
;; how to express the idea "apply m times the procedure"
;; (define (add m n)
;;   (lambda (f) (lambda (x) (f ..m.. (f (f ((n f) x)))))))

;; now lets take an look at
(define two (lambda (f) (lambda (x) (f (f x)))))
;; it is an anonymous procedure which takes a procedure f
;; and return a procedure with one argument x and apply
;; 2 times f to its argument x

;; substitute 2 times with m times,
;; this is the procedure exactly what we are looking for
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; a helper to display church numbers
(define (print-num num)
  ((num (lambda (x) (display "|"))) num))

;; this is adapted from http://d.hatena.ne.jp/tanakaBox/20070723/1186253252
(define (to-s num)
  ((num (lambda (n) (+ 1 n))) 0))

(to-s zero)
(to-s one)
(to-s two)
(to-s (add-1 two))
(to-s (add one (add two two)))

;; following the concepts of a number procedure,
;; a mul could be defined easily
;; apply the procedure n times, and apply the return procedure
;; m times and apply the resulting procedure with argument x
(define (mul m n)
  (lambda (f) (lambda (x) ((n (m f)) x))))

(to-s (mul two (mul (mul two two) (mul two two))))

;; now let's find the expt
;; apply n times m to f, and then apply that on x
;; n times m is expressed as (n m)
(define (expt m n)
  (lambda (f) (lambda (x) (((n m) f) x))))

(to-s (expt two (add two two)))



;; an example with inexact intervals

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (upper-bound y)))))

;; Exercise 2.7
(define (make-interval x y) (cons x y))
(define (upper-bound a) (max (car a) (cdr a)))
(define (lower-bound a) (min (car a) (cdr a)))
(define (print-interval interval)
  (display "[")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]")
  (newline))

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 2.58 2.97))

(print-interval r1)
(print-interval (add-interval r1 r2))
(print-interval (mul-interval r1 r2))

;; Exercise 2.8
(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y))
                  (- (lower-bound y)))))

(print-interval (sub-interval r1 r2))

;; Exercise 2.9
(define (interval-width a)
  (/ (- (upper-bound a) (lower-bound a)) 2.0))

(interval-width r2)
;; the result holds for width of the sum of two intervals
(- (+ (interval-width r1) (interval-width r2))
   (interval-width (add-interval r1 r2)))

;; but not multiplication and division
(- (+ (interval-width r1) (interval-width r2))
   (interval-width (mul-interval r1 r2)))

(- (+ (interval-width r1) (interval-width r2))
   (interval-width (div-interval r1 r2)))

;; Exercise 2.10
(define (span-zero? a)
  (or (= 0 (upper-bound a))
      (= 0 (lower-bound a))
      (negative? (* (upper-bound a) (lower-bound a)))))

(define (div-interval x y)
  (if (span-zero? y)
      (error "divider couldn't span over zero" (lower-bound y) (upper-bound y))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; (div-interval r1 (make-interval -1 1))

;; Exercise 2.11
;; one interval could be [-, -], [-, +], [+, +]
;; so a binary operation would have nine cases
;; the only condition need multiple multiplications are [-, +] with [-, +]
;; this makes the code very complex right now...

(define (gt-zero? a)
  (positive? (lower-bound a)))
(define (lt-zero? a)
  (negative? (upper-bound a)))

(define (mul-interval-new x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (gt-zero? x) (gt-zero? y))
           (make-interval (* xl yl) (* xu yu)))
          ((and (gt-zero? x) (span-zero? y))
           (make-interval (* xu yl) (* xu yu)))
          ((and (gt-zero? x) (lt-zero? y))
           (make-interval (* xu yl) (* xl yu)))
          ((and (lt-zero? x) (gt-zero? y))
           (make-interval (* xl yu) (* xu yl)))
          ((and (lt-zero? x) (span-zero? y))
           (make-interval (* xl yu) (* xu yl)))
          ((and (lt-zero? x) (lt-zero? y))
           (make-interval (* xu yu) (* xl yl)))
          ((and (span-zero? x) (lt-zero? y))
           (make-interval (* xu yl) (* xl yl)))
          ((and (span-zero? x) (gt-zero? y))
           (make-interval (* xl yu) (* xu yu)))
          ((and (span-zero? x) (span-zero? y))
           (make-interval (min (* xl yu) (* xu yl))
                          (max (* xl yl) (* xu yu)))))))

 (define (eql-interval? a b)
   (and (= (upper-bound a) (upper-bound b))
        (= (lower-bound a) (lower-bound b))))

 ;; Fails if the new mult doesn't return the same answer as the old
 ;; naive mult.
 (define (ensure-mult-works aH aL bH bL)
   (let ((a (make-interval aL aH))
         (b (make-interval bL bH)))
   (if (eql-interval? (mul-interval-new a b)
                      (mul-interval a b))
       #t
       (error "new mult returns different value!"
              a
              b
              (mul-interval-new a b)
              (mul-interval a b)))))

 (ensure-mult-works  +10 +10   +10 +10)
 (ensure-mult-works  +10 +10   +00 +10)
 (ensure-mult-works  +10 +10   +00 +00)
 (ensure-mult-works  +10 +10   +10 -10)
 (ensure-mult-works  +10 +10   -10 +00)
 (ensure-mult-works  +10 +10   -10 -10)
 (ensure-mult-works  +00 +10   +10 +10)
 (ensure-mult-works  +00 +10   +00 +10)
 (ensure-mult-works  +00 +10   +00 +00)
 (ensure-mult-works  +00 +10   +10 -10)
 (ensure-mult-works  +00 +10   -10 +00)
 (ensure-mult-works  +00 +10   -10 -10)
 (ensure-mult-works  +00 +00   +10 +10)
 (ensure-mult-works  +00 +00   +00 +10)
 (ensure-mult-works  +00 +00   +00 +00)
 (ensure-mult-works  +00 +00   +10 -10)
 (ensure-mult-works  +00 +00   -10 +00)
 (ensure-mult-works  +00 +00   -10 -10)
 (ensure-mult-works  +10 -10   +10 +10)
 (ensure-mult-works  +10 -10   +00 +10)
 (ensure-mult-works  +10 -10   +00 +00)
 (ensure-mult-works  +10 -10   +10 -10)
 (ensure-mult-works  +10 -10   -10 +00)
 (ensure-mult-works  +10 -10   -10 -10)
 (ensure-mult-works  -10 +00   +10 +10)
 (ensure-mult-works  -10 +00   +00 +10)
 (ensure-mult-works  -10 +00   +00 +00)
 (ensure-mult-works  -10 +00   +10 -10)
 (ensure-mult-works  -10 +00   -10 +00)
 (ensure-mult-works  -10 +00   -10 -10)
 (ensure-mult-works  -10 -10   +10 +10)
 (ensure-mult-works  -10 -10   +00 +10)
 (ensure-mult-works  -10 -10   +00 +00)
 (ensure-mult-works  -10 -10   +10 -10)
 (ensure-mult-works  -10 -10   -10 +00)
 (ensure-mult-works  -10 -10   -10 -10)

;; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

(make-center-percent 3.5 (/ 0.15 3.5))

;; Exercise 2.13
(define ix (make-center-percent 400 0.0001))
(define iy (make-center-percent 10 0.001))
(percent (mul-interval ix iy))

;; prove
;; i* stands for the interval
;; c* stands for the center of interval
;; p* stands for the percent of interval

(define iz (mul-interval ix iy))
;; when ix, iy only contains positive integers
(define iz (make-interval
            (* (lower-bound ix) (lower-bound iy))
            (* (upper-bound ix) (upper-bound iy))))

(define iz (make-interval
            (* (- cx (* cx px)) (- cy (* cy py)))
            (* (+ cx (* cx px)) (+ cy (* cy py)))))

;; now calculate width of iz
;; (width iz)
;; (/ (- (* (+ cx (* cx px)) (+ cy (* cy py)))
;;       (* (- cx (* cx px)) (- cy (* cy py))))
;;    2)
;; ((* cy (* cx px)) + (* cx (* cy py)))
;; (* cy cx (+ px py))
;; this is (width iz)

;; now calculate the center of iz
;; (center iz)
;; (/ (+ (* (+ cx (* cx px)) (+ cy (* cy py)))
;;       (* (- cx (* cx px)) (- cy (* cy py))))
;;    2)
;; (+ (* cx cy) (* px py))
;; because tolerence is significantly small, (* px py) could be omitted
;; (* cx cy)
;; this is (center iz)

;; calculate the percent of iz
;; (percent iz)
;; (/ (width iz) (center iz))
;; (/ (* cx cy (+ px py)) (* cx cy))
;; (+ px py)

;; so pz = px + py

;; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))


(par1 r1 r2)
;; => (1.5109665071770333 . 2.553517241379311)
(par2 r1 r2)
;; => (1.814896551724138 . 2.1258947368421057)

(define (print-interval-center-percent interval)
  (display (center interval))
  (display " ± ")
  (display (* 100 (percent interval)))
  (display "%")
  (newline))

(define (test-intervals list-of-intervals)
  (for-each (lambda (pair)
              (let ((a (car pair))
                    (b (cadr pair)))
                (display "a: ")
                (print-interval-center-percent a)
                (display "b: ")
                (print-interval-center-percent b)
                (display "a/a: ")
                (print-interval-center-percent (div-interval a a))
                (display "a/b: ")
                (print-interval-center-percent (div-interval a b))
                (display "a*a: ")
                (print-interval-center-percent (mul-interval a a))
                (display "a*b: ")
                (print-interval-center-percent (mul-interval a b))
                (newline)))
            list-of-intervals))

(test-intervals (map (lambda (tolerance-p)
                       (list (make-center-percent 100 (car tolerance-p))
                             (make-center-percent 200 (cadr tolerance-p))))
                     '((0.01 0.01)
                       (0.01 0.1)
                       (0.01 0.2)
                       (0.1 0.2)
                       (0.2 0.2)
                       (0.3 0.2)
                       (0.4 0.2)
                       (0.5 0.2)
                       )))

;; Conclusion
;; 1. the operation a / a will double the tolerance of the interval
;; 2. when p is smaller enough, the operation a / b will result in a tolerance which is the sum of
;;    the tolerance of both operands, p = pa + pb

;; Exercise 2.15

;; yes, it is correct.

;; par 1(/ (* r1 r2) (+ r1 r2))
;; could be rewritten as
;; (* (/ r1 r1)
;;    (/ 1
;;       (+ (/ 1 r1)
;;          (/ 1 r2))) ;; par2
;;    (/ r2 r2))

;; from Exercise 2.13 / 2.14, we have shown that multiple and division operations
;; have an side effect to increase the tolerance of the result (if the tolerance is
;; significant small)

;; so par1 will result in tolerance than par2
;; it is the transformation introduces error in the arithmetic procedure of the system

;; Exercise 2.16
;; Already explained Exercise 2.15, the mathematically equivalent transformation
;; isn't equivalent from an arithmetic point of view in the system

;; To eliminate the error, the system must perform the div-interval and mul-interval
;; without introducing extra tolerance
;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
