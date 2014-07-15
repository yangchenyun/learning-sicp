#lang r5rs
(#%require (only racket/base current-process-milliseconds))
(#%require (only math/base random-natural))

(define (displayln expr)
  (display expr)
  (newline))

(define (square x) (* x x))

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

;; Exercise 1.22

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (letrec ((start (current-process-milliseconds))
           (result (prime? n))
           (end (current-process-milliseconds)))
    (if result (report-result n (- end start)) 'nil)
    result))

(define (report-result number elapsed-time)
  (display number)
  (display " is prime, used ")
  (display elapsed-time)
  (displayln " miliseconds"))

(define (search-for-primes number count)
  (cond
   ((= count 0) 'nil)
   (else
    (let ((is-prime (if (timed-prime-test number) 1 0)))
      (search-for-primes
       (if (even? number) (+ 1 number) (+ 2 number))
       (- count is-prime))))))

(define (benchmark floor ceiling)
  (cond ((<= floor ceiling)
         (newline)
         (display "Find the smallest prime larger than ")
         (displayln floor)
         (search-for-primes floor 3)
         (benchmark (* floor 10) ceiling))
      ))

;; (benchmark 100000000 10000000000000)
;; Find the smallest prime larger than 1000000000
;; 1000000007 is prime, used 1 miliseconds
;; 1000000009 is prime, used 1 miliseconds
;; 1000000021 is prime, used 1 miliseconds

;; Find the smallest prime larger than 10000000000
;; 10000000019 is prime, used 52 miliseconds
;; 10000000033 is prime, used 70 miliseconds
;; 10000000061 is prime, used 61 miliseconds

;; Find the smallest prime larger than 100000000000
;; 100000000003 is prime, used 186 miliseconds
;; 100000000019 is prime, used 189 miliseconds
;; 100000000057 is prime, used 184 miliseconds

;; Find the smallest prime larger than 1000000000000
;; 1000000000039 is prime, used 625 miliseconds
;; 1000000000061 is prime, used 621 miliseconds
;; 1000000000063 is prime, used 619 miliseconds

;; Find the smallest prime larger than 10000000000000
;; 10000000000037 is prime, used 1976 miliseconds
;; 10000000000051 is prime, used 1985 miliseconds
;; 10000000000099 is prime, used 1982 miliseconds

;; for input larger than 10000000000, the machine running time is compatible with the algorithms big O

;; Exercise 1.23

(define (next dividor)
  (if (= dividor 2)
      3
      (+ 2 dividor)))

;; an improved version, reduce the calculation by half
(define (smallest-divisor n)
  (define (divide? n test-dividor)
    (= 0 (remainder n test-dividor)))
  (define (find-divisor n test-dividor)
    (cond
     ((> (square test-dividor) n) n)
     ((divide? n test-dividor) test-dividor)
     (else (find-divisor n (next test-dividor)))))
  (find-divisor n 2))

;; (benchmark 1000000000 10000000000000)

;; Find the smallest prime larger than 1000000000
;; 1000000007 is prime, used 0 miliseconds
;; 1000000009 is prime, used 1 miliseconds
;; 1000000021 is prime, used 1 miliseconds

;; Find the smallest prime larger than 10000000000
;; 10000000019 is prime, used 27 miliseconds
;; 10000000033 is prime, used 27 miliseconds
;; 10000000061 is prime, used 26 miliseconds

;; Find the smallest prime larger than 100000000000
;; 100000000003 is prime, used 103 miliseconds
;; 100000000019 is prime, used 115 miliseconds
;; 100000000057 is prime, used 93 miliseconds

;; Find the smallest prime larger than 1000000000000
;; 1000000000039 is prime, used 312 miliseconds
;; 1000000000061 is prime, used 307 miliseconds
;; 1000000000063 is prime, used 309 miliseconds

;; Find the smallest prime larger than 10000000000000
;; 10000000000037 is prime, used 1010 miliseconds
;; 10000000000051 is prime, used 999 miliseconds
;; 10000000000099 is prime, used 1003 miliseconds

;; Exercise 1.24

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
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; use the fast-prime?, and the big O is not lg(n)
;; the improvement is very significant
(define (timed-prime-test n)
  (letrec ((start (current-process-milliseconds))
           (result (fast-prime? n 3))
           (end (current-process-milliseconds)))
    (if result (report-result n (- end start)) 'nil)
    result))

(benchmark 1000000000 10000000000000)

;; Find the smallest prime larger than 1000000000
;; 1000000007 is prime, used 2 miliseconds
;; 1000000009 is prime, used 0 miliseconds
;; 1000000021 is prime, used 0 miliseconds

;; Find the smallest prime larger than 10000000000
;; 10000000019 is prime, used 1 miliseconds
;; 10000000033 is prime, used 1 miliseconds
;; 10000000061 is prime, used 2 miliseconds

;; Find the smallest prime larger than 100000000000
;; 100000000003 is prime, used 0 miliseconds
;; 100000000019 is prime, used 1 miliseconds
;; 100000000057 is prime, used 0 miliseconds

;; Find the smallest prime larger than 1000000000000
;; 1000000000039 is prime, used 1 miliseconds
;; 1000000000061 is prime, used 0 miliseconds
;; 1000000000063 is prime, used 0 miliseconds

;; Find the smallest prime larger than 10000000000000
;; 10000000000037 is prime, used 0 miliseconds
;; 10000000000051 is prime, used 1 miliseconds
;; 10000000000099 is prime, used 0 miliseconds

;; Exercise 1.25

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod b exp m)
  (remainder (fast-expt b exp) m))

;; the old version would reduce the operands of square by calculate the remainder in the 'reduce' process
;; thus reduce the size of integer required to do * operations
;; (expmod 120 10 13)
;; (remainder (square (expmod 120 5 13)) 13)
;; (remainder (square (remainder (* 120 (expmod 120 4)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (expomod 120 2 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (expmod 120 2)) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (remainder (square (expmod 120 1 13)) 13)) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (expmod 120 0 13)) 13)) 13)) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 1) 13)) 13)) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 (remainder (square 9) 13)) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square (remainder (* 120 3) 13)) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 (remainder (square 9) 13)) 13)) 13)
;; (remainder (square (remainder (* 120 3) 13)) 13)
;; (remainder (square 9) 13)
;; 3

;; However, the new version using fast-expt will calculate the final result of exponent and make the remainder process very expensive

;; Exercise 1.26
;; use a explicit * procedure instead of calling square
(define (bad-expmod b exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (* (bad-expmod b (/ exp 2) m)
             (bad-expmod b (/ exp 2) m)
             )
          m))
        (else
         (remainder
          (* b (bad-expmod b (- exp 1) m))
          m))))

;; the explicit * procedure requires to calculate expmod procedure twice each steps,
;; thus, instead of cutting the problem halve every steps, it remains the same problem
;; size. So, in worst case, this process becomes linear

;; Exercise 1.27
(define (full-fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (do ((a (- n 1) (- a 1)))
      ((not (and (> a 0) (try-it a))) (= a 0))))

(full-fermat-test 561)
(full-fermat-test 6601)
(prime? 561)
(prime? 6601)

;; Exercise 1.28
