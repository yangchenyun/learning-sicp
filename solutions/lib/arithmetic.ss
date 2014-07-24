(define (average a b) (/ (+ a b) 2))
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define pi/4 (atan 1 1))
(define pi (* 4 pi/4))
(define -pi (- pi))
(define 2pi (* 2 pi))
