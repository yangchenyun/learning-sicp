#lang r5rs
(#%require racket/include)
(include "../lib/dispatch.ss")
(include "../lib/arithmetic.ss")

;; Exercise 3.1
(define (make-accumulator init)
  (let ((sum init))
    (lambda (x) ;; encapsulate the local state - sum
      (set! sum (+ sum x))
      sum)))

(define A (make-accumulator 5))
(A 10)
(A 10)

;; Exercise 3.2
(define (make-monitored proc)
  (let ((counter 0))
    (lambda (x)
      (if (eq? 'how-many-calls? x)
          counter
          (begin
            (set! counter (+ 1 counter))
            (proc x))))))

(define s (make-monitored sqrt))

(s 100)
(s 25)
(s 'how-many-calls?)

;; Exercise 3.3
(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (deny-access arg)
    "Incorrect password")

  (define (dispatch password m)
    (if (eq? password secret)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown method: MAKE-ACCOUNT")))
        deny-access))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'other-password 'deposit) 50)

;; Exercise 3.4
(define (call-the-cops)
  "The cop will save you.")

(define (make-account balance secret)
  (let ((failed-attempt 0))
    (define (inc-failed-attempt)
      (set! failed-attempt (+ 1 failed-attempt)))
    (define (reset-failed-attempt)
      (set! failed-attempt 0))

    (define (withdraw amount)
      (reset-failed-attempt)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)
      (reset-failed-attempt)
      (set! balance (+ balance amount))
      balance)

    (define (deny-access arg)
      (inc-failed-attempt)
      (if (>= failed-attempt 7)
          (call-the-cops)
          "Incorrect password"))

    (define (dispatch password m)
      (if (eq? password secret)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown method: MAKE-ACCOUNT")))
          deny-access))
    dispatch))

(define acc (make-account 100 'secret))
(define (eval-n-times n exp)
  (if (= n 1)
      (eval exp)
      (begin
        (eval exp)
        (eval-n-times (- n 1) exp))))

(eval-n-times 6 '((acc 'wrong-secret 'deposit) 10))
(eval-n-times 1 '((acc 'secret 'deposit) 10))
(eval-n-times 7 '((acc 'wrong-secret 'deposit) 10))


;; Exercise 3.5
(define (monte-carlo trials test)
  (define (monte-carlo-iter trials-left true-count)
    (if (= 0 trials-left)
        (/ true-count trials)
        (let ((test-result (if (test) 1 0)))
          (monte-carlo-iter (- trials-left 1)
                            (+ true-count test-result)))))
  (monte-carlo-iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random-real)))))

(define (estimate-integral trials pred x1 x2 y1 y2)
  (define (pred-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (pred x y)))
  (let ((width (- x2 x1))
        (height (- y2 y1)))
    (let ((area (* width height)))
      (* area (monte-carlo trials pred-test)))))

(define (unit-circle-predicate x y)
  (< (sqrt (+ (square y) (square x))) 1))

(estimate-integral 10000 unit-circle-predicate -1.0 1.0 -1.0 1.0)

;; Exercise 3.6
(define rand
  (let ((seed random-init))
    ;; a fixed method with a clojure variable
    (lambda (method)
      (define (generate)
        (set! seed (rand-update seed))
        seed)
      (define (reset init)
        (set! seed init)
        init)
      (cond ((eq? 'generate method) (generate))
            ((eq? 'reset method) reset)
            (else (error "RAND: unknown method" method))))))

(rand 'generate)
((rand 'reset) 10)

;; Exercise 3.7
(define peter-acc (make-account 100 'open-sesame))

(define (make-joint old-acc old-pass new-pass)
  (define withdraw
    (lambda (amount)
      ((old-acc old-pass 'withdraw) amount)))

  (define deposit
    (lambda (amount)
      ((old-acc old-pass 'deposit) amount)))

    (define (deny-access arg)
      "Incorrect password")

  (define (dispatch password m)
    (if (eq? password new-pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown method: MAKE-JOINT")))
        deny-access))
  dispatch)

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'deposit) 100)
((paul-acc 'rsebud 'withdraw) 99)

;; Exercise 3.8
;; f should have an internal state
(define f
  (let ((first-result '()))
    (lambda (x)
      (if (null? first-result)
          (begin
            (set! first-result x)
            first-result)
          0))))

(+ (f 1) (f 0))
(+ (f 0) (f 1))

;; Exercise 3.9
;; recursive version
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; global env
;;  -> factorial
;;     param: n
;;     body (if ... )

(factorial 6)

;; creates a new env E1, bind n to 6 and evaluate
;; (if (= n 1)
;;       1
;;       (* n (factorial (- n 1))))

;; leads to evaluation of
(factorial 5)

;; create another new env E2, bind n to 5 and evaluate the body again

;; so on and so forth, until evaluation of
(factorial 1)
;; creates a new env En, bind n to 1 and evaluate the body which returns 1


(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product
                   counter
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; global env
;;  -> factorial
;;     param: n
;;     body: (fact-iter 1 1 n)
;;  -> fact-iter
;;     param: product counter max-count
;;     body: (if ...)

(factorial 6)
;; creates a new env with n binds to 6 and evaluate
(fact-iter 1 1 n)

;; creates a new env, with product, counter, max-count bound to 1, 1, 6
;; and evaluate the body of `fact-iter'

;; Exercise 3.10
;; the evaluation of `let' will create a environment to hold initial-amount
;; and invoke of the anonymous closure will create an environment point to
;; the previous environment


;; Exercise 3.11
;; local procedures are defined within a clojure
