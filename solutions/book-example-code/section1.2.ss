#lang r5rs

(define (square x) (* x x))

;; 1.2.1

;; linear recursive process for factorial (n)
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; the local evolution of one process
(factorial 5)
(if (= 5 1) 1 (* 5 (factorial (- 5 1))))
(* 5 (factorial (4)))
(* 5 (* 4 (factorial 3)))
(* 5 (* 4 (* 3 (factorial 2))))
(* 5 (* 4 (* 3 (* 2 (factorial 1)))))
(* 5 (* 4 (* 3 (* 2 1))))
(* 5 (* 4 (* 3 2)))
(* 5 (* 4 6))
(* 5 24)
(120)

;; linear iterative process for factorial(n)
(define (factorial-i n)
  (define fact-iter
    (lambda (count product)
      (if (>= count n)
          (* product count)
          (fact-iter (+ count 1) (* product count)))))
  (fact-iter 1 1))

;; the local evolution of this process
(factorial-i 5)
;; (fact-iter 1 1)
;; (fact-iter 2 1)
;; (fact-iter 3 2)
;; (fact-iter 4 6)
;; (fact-iter 5 24)
;; (* 5 24)
;; (120)

;; 1.2.2
;; recursive process for fibonacci numbers
(define (fib n)
  (cond ((= n 1) 1)
        ((= n 0) 0)
        (else (+
               (fib (- n 1))
               (fib (- n 2))))))

;; iterative process
(define (fib-i n)
  (define (fib-iter a b count)
    (if (= count n)
        b
        (fib-iter
         b
         (+ a b)
         (+ count 1))))
  (fib-iter 0 1 1))

;; procedure to compute ways of exchange
;; algorithem
;; the number of ways to exchange amount a with n kinds of of coins equal:
;; - the number of ways to exchange amount a without the first coin (n-1) kinds
;; - the number of ways to exchange amount a - d with n kinds of coins
;; if a is 0, a change is established
;; if a < 0, doesn't success
;; if n is 0, doesn't success

(define (count-change amount)
  (define (coin-value n)
    (cond ((= n 5) 50)
          ((= n 4) 25)
          ((= n 3) 10)
          ((= n 2) 5)
          ((= n 1) 1)
          (else #f)))
  (define (cc-iter amount kinds-of-coin)
    (cond ((= amount 0) 1)
          ((= kinds-of-coin 0) 0)
          ((< amount 0) 0)
          (else
           (+ (cc-iter amount (- kinds-of-coin 1))
              (cc-iter
               (- amount (coin-value kinds-of-coin))
               kinds-of-coin)))))
  (cc-iter amount 5))

;; 1.2.3
(define (expt b n)
  (define (expt-iter b counter p)
    (if (= counter 0)
        p
        (expt-iter b (- counter 1) (* p b))))
  (expt-iter b n 1))

(expt 3 10)

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 10)

;; 1.2.5
(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 28 14)

;; 1.2.6

(define (smallest-divisor n)
  (define (divide? n test-dividor)
    (= 0 (remainder n test-dividor)))
  (define (find-divisor n test-dividor)
    (cond
     ((> (square test-dividor) n) n)
     ((divide? n test-dividor) test-dividor)
     (else (find-divisor n (+ 1 test-dividor)))))
  (find-divisor n 2))


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
