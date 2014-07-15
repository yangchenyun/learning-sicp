#lang r5rs

(define (average a b) (/ (+ a b) 2))
(define (cube x) (* x x x))
;; 1.3.1 Procedures as Arguments

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 10 20)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(sum-cubes 10 13)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(pi-sum 3 11)

;; abstract the repeated pattern with a new concept
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; now redefine the above three functions
(define (inc n) (+ n 1))

(define (sum-integers a b)
  (sum + a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; use the new `sum' concept to build the integral function
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.0001)

;; 1.3.3
;; procedures to find the roots of equation
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

;; procedures to find fixed point
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

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;; this doesn't converge
;; (define (sqrt x)
;;   (fixed-point (lambda (y) (/ x y)) 1.0))

;; to adjust the changes of every guessing
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

;; the three ideas are explicit: fixed-point, average-damp, and y = x/y
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

;; express the idea of derivative by a process, which transform a function
;; into another function
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-methods g guess)
  (fixed-point (newton-transform g) guess))

;; to calculate sqrt is to find the root of g(y) = y^2 - x = 0
(define (sqrt x)
  (newton-methods
   (lambda (y) (- (square y) x)) 1.0))

;; abstract out the idea: find the fixed point of some transformation of a function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; redefine sqrt using the new concept
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- x (square y))) newton-transform 1.0))
