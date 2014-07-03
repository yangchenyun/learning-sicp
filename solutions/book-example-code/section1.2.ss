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
(define (factorial n)
  (define fact-iter
    (lambda (count product)
      (if (>= count n)
          (* product count)
          (fact-iter (+ count 1) (* product count)))))
  (fact-iter 1 1))

;; the local evolution of this process
(factorial 5)
(fact-iter 1 1)
(fact-iter 2 1)
(fact-iter 3 2)
(fact-iter 4 6)
(fact-iter 5 24)
(* 5 24)
(120)

;; 1.2.2
;; recursive process for fibonacci numbers
(define (fib n)
  (cond ((= n 1) 1)
        ((= n 0) 0)
        (else (+
               (fib (- n 1))
               (fib (- n 2))))))

;; iterative process
(define (fib n)
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
           (+ (cc-iter amount (-1+ kinds-of-coin))
              (cc-iter
               (- amount (coin-value kinds-of-coin))
               kinds-of-coin)))))
  (cc-iter amount 5))