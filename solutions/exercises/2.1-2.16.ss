#lang r5rs

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
(car (cons x y))
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

;; Now make another guess for `add'
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

(print-num one)
(print-num two)
(print-num (add-1 two))
(print-num (add one (add two two)))
