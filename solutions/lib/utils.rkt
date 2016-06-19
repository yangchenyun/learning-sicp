#lang racket
(provide (all-defined-out))

;; utility
(define (compose f g)
  (define (f*g x)
    (f (g x)))
  f*g)

(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

(define (identity x) x)

(define (displayln list)
  (display list)
  (newline))
