;; Exercise 1.9
(define (square x) (* x x))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; recursive process
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(if (= 4 5) 5 (inc (+ (dec 4) b)))
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
(9)

;; iterative process
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
(9)

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; ...
;; (A 0 ... (A 1 1))
;; 2^10

(A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 4))
;; (A 1 16)
;; 2^16

(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 4)
;; 2^16

(define (f n) (A 0 n))
;; O(0)
;; f(n) = 2n
;; could be reduced to
(define (f n) (* 2 n))

(define (g n) (A 1 n))
;; O(n)
;; g(n) = 2^n
;; could be reduced to
(define (g n)
  (if (= n 1)
      2
      (* 2 (g (- n 1)))))

(define (h n) (A 2 n))
;; O() ?
;; h(n) = 2^h(n-1)

;; Challenge: write the coin-exchange in iterative process

;; Challenge: write the tower-move in iterative process

;; Exercise 1.11

;; recursive process
(define (func n)
  (if (< n 3)
      n
      (+
       (* 1 (func (- n 1)))
       (* 2 (func (- n 2)))
       (* 3 (func (- n 3))))))

;; iterative process
(define (func n)
  (define (func-iter a b c count)
    (if (= count n)
        (+ a (* 2 b) (* 3 c))
        (func-iter
         (+ a (* 2 b) (* 3 c))
         a
         b
         (+ count 1))))
  (if (< n 3)
      n
      (func-iter 2 1 0 3)))

;; Exercise 1.12
;; Pascal’s triangle
(define (pascal row column)
  (cond ((< row column) 0)
        ((or (= row column) (= column 1)) 1)
        (else
         (+
          (pascal (- row 1) (- column 1))
          (pascal (- row 1) column)))))

;; Exercise 1.14

;; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1)) angle
      (p (sine (/ angle 3.0)))))

;; everytime p applies, angle is divided by 3
(define (count-div-three-until-small-enough n)
  (if (< (/ n 3) 0.1) 1
      (+ 1 (count-div-three-until-small-enough (/ n 3)))))
(count-div-three-until-small-enough 12.15)

;; so p will be applied 5 times

(sine a)
(p (sine (/ angle 3.0)))
(p (sine (p (sine (/ (/ angle 3.0) 3.0)))))

;; number of steps: O(lg3a), every step will divide the number by 3
;; space: O(lg3a), new spaces are needed to hold the recursive part to calculate the x in procedure p

;; Exercise 1.16
;; iterative process with log(n) order of growth to calculate exponents
(define (fast-expt b n)
  (define (square x) (* x x))
  (define (expt-iter b n result)
    (cond
     ((= n 0) result)
     ((even? n) (expt-iter (square b) (/ n 2) result))
     (else (expt-iter b (- n 1) (* result b)))))
  (expt-iter b n 1))

;; Exercise 1.17
(define (fast-mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond
   ((= b 1) a)
   ((even? b) (double (fast-mul a (halve b))))
   (else (+ a (fast-mul a (- b 1))))))

;; Exercise 1.18
(define (fast-mul-i a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (mul-iter a b result)
    (cond
     ((= b 1) (+ a result))
     ((even? b) (mul-iter (double a) (halve b) result))
     (else (mul-iter a (- b 1) (+ result a)))))
  (mul-iter a b 0))

;; Exercise 1.19
(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   q ; compute p′ = q
                   (+ p q) ; compute q′ = p + 1
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
;; applied in normal order evaluation
(gcd 40 (remainder 206 40))
;; evals (remainder 206 40) => 6, c = 1

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; evals again (remainder 40 (remainder 206 40)) => 4, c = 2

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; evals again (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) => 2, c = 4

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; evals again, => 0, c = 7

(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; => 2, c = 4

;; so in total the evaluation count is 1 + 2 + 4 + 7 + 4 = 18

;; applied in applicative-order evaluation
(gcd 40 (remainder 206 40)) ;; c = 1
(gcd 40 6)
(gcd 6 (remainder 40 6)) ;; c = 1
(gcd 6 4)
(gcd 4 (remainder 6 4)) ;; c = 1
(gcd 4 2)
(gcd 2 (remainder 4 2)) ;; c = 1
(gcd 2 0)
2

;; so in total the evaluation is 4

;; Exercise 1.21
(define (smallest-divisor n)
  (define (divide? n test-dividor)
    (= 0 (remainder n test-dividor)))
  (define (find-divisor n test-dividor)
    (cond
     ((> (square test-dividor) n) n)
     ((divide? n test-dividor) test-dividor)
     (else (find-divisor n (+ 1 test-dividor)))))
  (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;;
