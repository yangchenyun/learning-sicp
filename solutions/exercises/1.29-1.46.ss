#lang r5rs
(#%require racket/include)
(include "../lib/arithmetic.ss")

;; Exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (letrec ((h (/ (- b a) n))
           (kth (lambda (u) (/ (- u a) h))) ;; get the kth
           (next (lambda (u) (+ u h)))
           (term (lambda (u)
                   (cond ((or (= (kth u) 0) (= (kth u) n)) (f u)) ;; first and last item
                         ((even? (kth u)) (* 2.0 (f u)))
                         (else (* 4.0 (f u)))))))
    (* (/ h 3) (sum term a next b))))

(simpson-integral cube 0 1 10000)

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31
(define (prod term a next b)
  (if (> a b)
      1
      (* (term a) (prod term (next a) next b))))

(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (prod (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

;; O(n^2), term is executed n times and * is executed n times
(define (prod-pi n)
  (* 4.0 (prod-iter (lambda (i)
                      (cond ((= i 1) (/ 2 3))
                            (else (/ (+ (* 2 (floor (/ i 2))) 2)
                                     (+ (* 2 (floor (/ (+ 1 i) 2))) 1)))))
                    1 (lambda (x) (+ 1 x)) n)))

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (prod term a next b)
  (accumulate * 1 term a next b))

;; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ;; only accumulate when filter returns true
        ((filter a) (combiner (term a)
                              (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (smallest-divisor n)
  (define (divide? n test-dividor)
    (= 0 (remainder n test-dividor)))
  (define (find-divisor n test-dividor)
    (cond
     ((> (square test-dividor) n) n)
     ((divide? n test-dividor) test-dividor)
     (else (find-divisor n (+ 1 test-dividor)))))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-square-sum a b)
  (filtered-accumulate + 0 square a (lambda (x) (+ x 1)) b prime?))

(define (relative-prime-prod n)
  (filtered-accumulate * 1 (lambda (x) x)
                       1 (lambda (x) (+ x 1)) n
                       (lambda (i) (= 1 (gcd i n)))))

;; Exercise 1.34
(define (f g) (g 2))
(f square)

;; (f f)
;; (f 2)
;; (2 2)
;; as 2 is not a procedure it would raise an issue

;; Exercise 1.35
(define tolerence 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? u v)
    (< (abs (- u v)) tolerence))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; Exercise 1.36
(define (fixed-point-with-print f first-guess)
  (define (close-enough? u v)
    (< (abs (- u v)) tolerence))
  (define (try guess number-of-attempt)
    (display number-of-attempt)
    (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 number-of-attempt)))))
  (try first-guess 0))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

;; with average-damp, it converges faster
(fixed-point-with-print (lambda (x) (/ (log 1000) (log x))) 10.0)
(fixed-point-with-print (average-damp (lambda (x) (/ (log 1000) (log x)))) 10.0)

;; Exercise 1.37

(define (cont-frac n d k)
  (define nextn
    (lambda (i) (n (+ i 1))))
  (define nextd
    (lambda (i) (d (+ i 1))))
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n 1) (+ (d 1)
                  (cont-frac nextn nextd (- k 1))))))

(define golden-ratio 0.61803398874989484820458683436565)

(define (steps-for-precision precision)
  (define (try n)
    (let ((estimate (cont-frac (lambda (i) 1.0)
                               (lambda (i) 1.0) n)))
      (if (< (abs (- golden-ratio estimate)) precision)
          n
          (try (+ n 1)))))
  (try 1))

(steps-for-precision 0.0001) ;; => 10

(define (cont-frac-iter n d k)
  (define (iter i frac)
    (if (= i 0)
        frac
        ;; accumulate on the frac part
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) frac)))))
  (iter (- k 1) (/ (n k) (d k))))

;; Exercise 1.38
(define euler-number  2.7182818284590452353602874)
(define (euler-number-cf steps)
  (cont-frac (lambda (i) 1)
             (lambda (i)
               (if (= (modulo i 3) 2)
                   (* 2.0 (ceiling (/ i 3))) 1)) steps))

(- (- euler-number 2)
   (euler-number-cf 10))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1.0)) k))

(- (tan-cf 10 100) (tan 10))

(define pi 3.14159265)
(tan-cf (/ pi 4) 10)

;; Exercise 1.40

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-methods g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newton-methods (cubic 0 0 -1.0) 1.0)

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define inc (lambda (x) (+ 1 x)))

((double inc) 1)
(((double (double double)) inc) 5)

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44
(define (smoothing f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smoothing f n)
  (repeated smoothing n) f)

;; Exercise 1.45
(define (log2-floor n)
  (define (iter m count)
    (if (= m 0)
        (- count 1)
        (iter (arithmetic-shift m -1) (+ 1 count))))
  (iter n 0))

(define (nth-root x n)
  ;; assume log2(n) average-damp is required
  (fixed-point ((repeated average-damp (log2-floor n))
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nth-root 64 6)
(nth-root 1000000 6)

;; Exercise 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x)) 0.0001))
                      (lambda (guess)
                        (average guess (/ x guess)))) 1.0))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- guess (f guess))) 0.0001))
                      (lambda (guess)
                        (f guess))) first-guess))
