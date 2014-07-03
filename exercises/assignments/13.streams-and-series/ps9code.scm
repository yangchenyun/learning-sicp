;;;; 6.001 Fall 1995
;;;; This is the file ps9code.scm

;;; The following definitions install rational arithmetic.  Warning:
;;; Don't use any arithmetic operations other than these.

(define + (access + '()))
(define - (access - '()))
(define * (access * '()))
(define / (access / '()))

;;; some basic stream operations

;; the empty stream is the same as the empty list in our implementation
;; of streams
(define the-empty-stream '())

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (add-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream (+ (stream-car s1) (stream-car s2))
                      (add-streams (stream-cdr s1)
                                   (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-car s))
	     (show-series (stream-cdr s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)
  (stream-ref s n))



;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (cons-stream 0 zeros))
  (define (iter list)
    (if (null? list)
	zeros
	(cons-stream (car list)
		     (iter (cdr list)))))
  (iter list-of-coeffs))


;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....


(define (proc->series proc)
  (stream-map proc non-neg-integers))


