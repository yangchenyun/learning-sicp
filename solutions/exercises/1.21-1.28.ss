#lang sicp

(#%require (only racket/base
                 displayln
                 current-process-milliseconds))
(#%require (only math/base random-natural))
(#%require racket/include)
(include "../lib/arithmetic.rkt")

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

;; (benchmark #e1e11 #e1e16)
;; Find the smallest prime larger than 100000000000
;; 100000000003 is prime, used 8 miliseconds
;; 100000000019 is prime, used 7 miliseconds
;; 100000000057 is prime, used 6 miliseconds

;; Find the smallest prime larger than 1000000000000
;; 1000000000039 is prime, used 20 miliseconds
;; 1000000000061 is prime, used 20 miliseconds
;; 1000000000063 is prime, used 20 miliseconds

;; Find the smallest prime larger than 10000000000000
;; 10000000000037 is prime, used 65 miliseconds
;; 10000000000051 is prime, used 69 miliseconds
;; 10000000000099 is prime, used 65 miliseconds

;; Find the smallest prime larger than 100000000000000
;; 100000000000031 is prime, used 200 miliseconds
;; 100000000000067 is prime, used 198 miliseconds
;; 100000000000097 is prime, used 202 miliseconds

;; Find the smallest prime larger than 1000000000000000
;; 1000000000000037 is prime, used 630 miliseconds
;; 1000000000000091 is prime, used 627 miliseconds
;; 1000000000000159 is prime, used 618 miliseconds

;; Find the smallest prime larger than 10000000000000000
;; 10000000000000061 is prime, used 1985 miliseconds
;; 10000000000000069 is prime, used 1991 miliseconds
;; 10000000000000079 is prime, used 1981 miliseconds

;; for input larger than 1e11, the machine running time is
;; compatible with the algorithms big O

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

(benchmark #e1e11 #e1e16)

;; Find the smallest prime larger than 100000000000
;; 100000000003 is prime, used 5 miliseconds
;; 100000000019 is prime, used 4 miliseconds
;; 100000000057 is prime, used 4 miliseconds

;; Find the smallest prime larger than 1000000000000
;; 1000000000039 is prime, used 13 miliseconds
;; 1000000000061 is prime, used 13 miliseconds
;; 1000000000063 is prime, used 13 miliseconds

;; Find the smallest prime larger than 10000000000000
;; 10000000000037 is prime, used 45 miliseconds
;; 10000000000051 is prime, used 45 miliseconds
;; 10000000000099 is prime, used 41 miliseconds

;; Find the smallest prime larger than 100000000000000
;; 100000000000031 is prime, used 132 miliseconds
;; 100000000000067 is prime, used 130 miliseconds
;; 100000000000097 is prime, used 130 miliseconds

;; Find the smallest prime larger than 1000000000000000
;; 1000000000000037 is prime, used 428 miliseconds
;; 1000000000000091 is prime, used 424 miliseconds
;; 1000000000000159 is prime, used 418 miliseconds

;; Find the smallest prime larger than 10000000000000000
;; 10000000000000061 is prime, used 1358 miliseconds
;; 10000000000000069 is prime, used 1360 miliseconds
;; 10000000000000079 is prime, used 1350 miliseconds

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

(benchmark #e1e90 #e1e100)

;; The improvement is fundamental, now it is less then 1m to calculate
;; input larger than 1e100, the machine running time is compatible
;; with the algorithms big O

;; Exercise 1.25

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; (define (expmod b exp m)
;;   (remainder (fast-expt b exp) m))

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

(define (robin-miller-test n)
  (define (nontrivial-sqrt-check i)
    (if (and (= 1 (remainder (square i) n))
          (> i 1)
          (< i (- n 1)))
        0
        i))
  (define (expmod b exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square
                       (nontrivial-sqrt-check
                        (expmod b (/ exp 2) m)))
                      m))
          (else
           (remainder
            (* b (expmod b (- exp 1) m))
            m))))
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (repeat-test test n)
  (lambda (x)
    (if (> n 1)
        (and (test x) ((repeat-test test (- n 1)) x))
        (test x))))

(define (test-all-numbers test numbers)
  (map (lambda (i) (test i)) numbers))

(test-all-numbers
 (repeat-test robin-miller-test 10)
 '(561 1105 1729 2465 2821 6601 6601 941 947 953 967 971 977 983 991 997))
