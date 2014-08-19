#lang r5rs

;; new special form for the assignment operator
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 10)
(withdraw 30)
(withdraw 50)

;; encapsulate the balance variable
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define withdraw1 (make-withdraw 100))
(withdraw1 50)
(withdraw1 60)

;; add the deposit concept
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown method: MAKE-ACCOUNT"))))
  dispatch)

(define ACC (make-account 100))
((ACC 'withdraw) 60)
((ACC 'deposit) 90)

;; the design of a procedure `rand'
;; assuming we have a `rand-update'
;; http://en.wikipedia.org/wiki/Lehmer_random_number_generator
(define (rand-update i)
  (let ((n 65537)
        (b 1299689)
        (g 75))
    (remainder (* g i) n)))

(define (int-enumerator max)
  (if (= 0 max)
      '()
      (cons max (int-enumerator (- max 1)))))

;; an encapsulated version
(define random-init 100)
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials test)
  (define (monte-carlo-iter trials-left true-count)
    (if (= 0 trials-left)
        (/ true-count trials)
        (let ((test-result (if (test) 1 0)))
          (monte-carlo-iter (- trials-left 1)
                            (+ true-count test-result)))))
  (monte-carlo-iter trials 0 0))

;; an version using `rand-update' directly
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials 10))))

(define (random-gcd-test trials rand-seed)
  (define (iter trials-left true-count x)
    (if (= 0 trials-left)
        (/ true-count trials)
        (let* ((x1 (rand-update x))
               (x2 (rand-update x1))
               (test-result (if (= 1 (gcd x1 x2)) 1 0)))
          (iter (- trials-left 1)
                (+ true-count test-result)
                x2))))
  (iter trials 0 rand-seed))

;; now the implementation details of `rand-update' is all over places
