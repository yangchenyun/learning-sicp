#lang r5rs
(require racket/include)
(include "../lib/arithmetic.ss")

;; pretty format
(+ (* 3
       (+ (* 2 4)
          (+ 3 5)))
   (+ (- 10 7)
      6))

;; naming
(define size 4)

(define pi 3.1415926)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))

;; procedure definition
(define (square x) (* x x))

(define (sum-of-square x y) (+ (square x) (square y)))

(define (f a)
  (sum-of-square
   (+ a 1)
   (+ a 2)))

;; Application Order Evaluation
(f 9)
; 1. Evaluate with the body of procedures with ...
(sum-of-square (+ a 1) (+ a 2))
; ... with formal parameters replaced by corresponding arguments
(sum-of-square (+ 9 1) (+ 9 2))
(sum-of-square 10 11)
(+ (square 10) (square 11))
(+ (* 10 10) (* 11 11))
(+ 100 121)
221

;; Normal Order Evaluation
(f 9)
; would not evaluate the operands until the value is needed
(sum-of-square (+ a 1) (+ a 2))
(+ (square (+ a 1)) (square (+ a 2)))
(+ (* (+ a 1) (+ a 1)) (* (+ a 2) (+ a 2)))
(+ (* 10 10) (* 11 11))
(+ 100 121)
221

;; conditional expression
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) x)
        ((< x 0) (- x))))

;; with else
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; with if
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; compound predicate
(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y) (not (< x y)))


;; sqrt - the Newton's method
(define sqrt_iter (lambda (guess x)
               (if (good_enough? guess x)
                   guess
                   (sqrt_iter (improve guess x) x))))
(define (good_enough? guess x)
  (<
   (abs (- (square guess) x))
   0.0001))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

;; procedural abstraction
(define (square x) (exp (double (log x))))
(define (double x) (+ x x))

;; sqrt in a block structure
(define (sqrt x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (/
        (abs (- guess (improve guess x)))
        guess)
       0.00001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))


;; x is a free variable in the internal definition
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (/
        (abs (- guess (improve guess)))
        guess)
       0.0001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
