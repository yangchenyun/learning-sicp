#lang r5rs

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
     (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; definition of the constructor and selector
(define (make-rat n d) (cons n d))
(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat (add-rat one-third one-half))
(print-rat (mul-rat one-third one-half))
(print-rat (sub-rat one-third one-half))
(print-rat (add-rat one-third one-third))

;; improve make-rat to reduce numbers to lowest terms
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Definition of data
;; collections of selector and constructor, together with specified conditions
;; those conditions could be expressed as:
;; 1. in terms of previously defined types of data (Hoare, 1972)
;; 2. in terms of algebraic procedures (Guttag, 1977)

;; We could define the 'pair' data with pure procedures

(define (cons x y)
  ;; notice dispatch is defined in a clojure of x, y
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car p) (p 0))
(define (cdr p) (p 1))
