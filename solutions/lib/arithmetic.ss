(define (average a b) (/ (+ a b) 2))
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
