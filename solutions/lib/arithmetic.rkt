;; TODO(steveyang): turned it into a racket module instead of
;; snippet to be included.

(#%require (only racket/base random))

(define (average a b) (/ (+ a b) 2))
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define pi/4 (atan 1 1))
(define pi (* 4 pi/4))
(define -pi (- pi))
(define 2pi (* 2 pi))

(define (expmod b exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod b (/ exp 2) m))
          m))
        (else
         (remainder
          (* b (expmod b (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 3))

(#%require srfi/27)
(define (rand-update i)
  (let ((n 65537)
        (b 1299689)
        (g 75))
    (remainder (* g i) n)))
(define random-init 10)
