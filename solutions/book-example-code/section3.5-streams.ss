#lang racket
(require r5rs)
(load "../lib/stream.ss")
(load "../lib/arithmetic.ss")

(stream-car
 (stream-cdr
  (stream-filter
   prime? (stream-enumerate-interval
           10000 1000000))))

;; infinite streams
(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= 0 (remainder x y)))

(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7))) integers))

(stream-ref no-sevens 100)

;; the stream generator like one step in iterative recursion
(define (fib-gen a b)
  (cons-stream a (fib-gen b (+ a b))))

(define fibs (fib-gen 0 1))

(stream-ref fibs 10)

(define (sieve stream)
  (let ((prime (stream-car stream)))
    (cons-stream
     prime
     (sieve (stream-filter (lambda (x)
                       (not (divisible? x prime)))
                     (stream-cdr stream))))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

;; define stream implicitly

(define ones (cons-stream 1 ones))
(define (add-stream s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-stream ones integers)))

(stream-ref integers 20)

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-stream (stream-cdr fibs) fibs))))

(stream-ref fibs 10)

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
              s))

(define double (cons-stream 1 (scale-stream double 2)))

(stream-ref double 10)

;; the stream paradigm

;; streams for sqrt
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess)
                   (sqrt-improve guess x)) guesses)))
  guesses)

(display-stream-first (sqrt-stream 2) 10)

;; streams for pi

;; 1, -1, 1, -2
(define alts
  (cons-stream 1 (neg-stream alts)))

;; compute the summands of the series
(define pi-summands
  (mul-streams alts
               (stream-map / ones (stream-filter odd? integers))))
(display-stream-first pi-summands 10)

(define pi-stream
  (scale-stream (partial-sums pi-summands) 4.0))

(display-stream-first pi-stream 10)

;; a sequence accelerator

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ;; Sn-1
        (s1 (stream-ref s 1))   ;; Sn
        (s2 (stream-ref s 2)))  ;; Sn+1
    (cons-stream
     (- s2
        (/ (square (- s2 s1))
           (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(display-stream-first (euler-transform pi-stream) 10)

;; we could create a tableau, a stream of streams

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-stream-first (accelerated-sequence euler-transform pi-stream) 8)

;; infinite stream pairs
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (append-streams
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

;; the first `stream-map' stream is infinite
(display-stream-first
 (pairs integers integers) 10)

(define (interleave s1 s2)
  (if (null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(display-stream-first
 (pairs integers integers) 10)

;; signal processing
(define (integral input initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream input dt)
                  int)))
  int)
