#lang sicp

(#%require (only racket/base
                 for
                 time displayln))
(#%require racket/include)
(include "../lib/arithmetic.rkt")

;; Exercise 2.3

;; Cannot find definitino of `my-sine' and `recurrent-sine', so only
;; evaluate the given definition below:

;; (define (my-sine x)
;;   (if (small-enuf? x)
;;       x
;;       (* (my-sine (/ x 3))
;;          (- 3
;;             (* 4 (my-sine (/ x 3))
;;                  (my-sine (/ x 3)))))))

;; For each step of recursion, x is divided by 3 and it recurs into
;; three applications; so the growth of operations and space is (3n *
;; n/3) = O(n)

(define (my-sine x)
  (if (< x 0.01)
      x
      (* (my-sine (/ x 3))
         (- 3
            (* 4 (my-sine (/ x 3))
                 (my-sine (/ x 3)))))))

;; The time O(n) is proved by:
(for ([x '(100.0 1000.0 10000.0)])
  (time (my-sine x)))
