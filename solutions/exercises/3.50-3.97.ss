#lang racket
(require r5rs)
(load "../lib/stream.ss")

;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(stream-car
 (stream-cdr
  (stream-cdr
   (stream-map +
               (stream-enumerate-interval 10 20)
               (stream-enumerate-interval 1 10)))))

;; Exercise 3.51
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map
   show
   (stream-enumerate-interval 0 10)))
;; would display 0

(stream-ref x 5)
;; would display 1, 2, 3, 4, 5
(stream-ref x 7)
;; would display 6, 7

;; explaination
;; `show' will display the result being evaluated

;; when defining x `stream-map' is evaluated, and return a promise of a stream-map
;; (cons
;;   (apply show (stream-enumerate-interval 0 10)) ;; this display 0
;;   (delay
;;     (apply stream-map show (stream-cdr (stream-enumerate-interval 0 10)))))

;; (cons 0
;;   (delay
;;     (apply stream-map show (stream-cdr (stream-enumerate-interval 0 10)))))

(stream-ref x 5)
;; would reduced to
(stream-car
 (stream-cdr
  (stream-cdr
   (stream-cdr
    (stream-cdr
     (stream-cdr x))))))

;; (stream-cdr x) will reduces to
;; (stream-cdr
;;  (cons 0
;;        (delay
;;          (apply stream-map show (stream-cdr (stream-enumerate-interval 0 10))))))

;; this will force the promise to be carried out and return
;; (stream-map show (stream-enumerate-interval 1 10))

;; this will display 1, and return another promise

;; every application of `stream-cdr' will force the `stream-map' to be evaluated
;; at next step

;; so (stream-ref 5) will evaluate the stream-map 5 times and print 1 - 5 and return
;; the result 5

;; when evaluating (stream-ref 7), as the first 6 values in the (stream-map ...) have
;; already been evaluated and memorized by `memo', it only needs to evaluate two
;; steps further, thus print 6 and 7

;; Exercise 3.52
;; in case `delay' is implemented with memorization
(define sum 0)
;; => void, sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
;; => void, sum = 0

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))
;; => void, sum = 1

;; when evaluating the definition, `accum' is evaluated once
;; (cons
;;  (accum (stream-car (stream-enumerate-interval 1 20)))
;;  (delay (stream-map accum (stream-cdr (stream-enumerate-interval 1 20)))))

(define y (stream-filter even? seq))
;; => void, sum = 6

;; (stream-car seq) is 1
;; (stream-filter even? seq)
;; will evaluated to:
;; (stream-filter even? (stream-cdr seq))
;; this will evaluate the `stream-map' one step further
;; (stream-filter even?
;;                (stream-map accum (stream-enumerate-interval 2 20)))
;; stream-map will return a stream with (+ x sum) as its first element
;; x 2 and sum is 1,now sum is 1 + 2 = 3
;; (stream-car seq) is 3
;; this will reduced to
;; (stream-filter even?
;;                (stream-map accum (stream-enumerate-interval 3 20)))
;; now (stream-car seq) and sum will be (+ 3 3) which is 6, and it returns
;; a stream object
;; (cons-stream 6 (stream-filter
;;                 even?
;;                 (stream-cdr (stream-map
;;                              accum
;;                              (stream-enumerate-interval 3 20)))))

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) seq))
;; `stream-filter' will continue evaluate the stream until the pred is true
;; in this case, it will evaluate until the first five elements of `stream-map'
;; 1, 3, 6, 10. The first three elements are already evaluated when defining y
;; so only the fourth will be evaluated, and `accum' will be called once

;; => void, sum = 10

(stream-ref y 7)
;; as the first even element is evaluated, 6
;; seven more even elements will be returned, 10, 28, 36, 66, 78, 120, 136
;; all required `stream-map' will be carried out,
;; 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;; 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136

;; => 136, sum = 136

(display-stream z)
;; the whole of stream will be examined and `accum' will be called
;; (output 10, 15, 45, 55, 105, 120... )
;; => void, sum = 210

;; if `delay' isn't implemented with memorization, every call to the stream
;; will be performed from scratch
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))
;; first of (stream-enumerate-interval 1 20) is evaluated
;; sum = 1

;; now the (stream-car seq) will always be 1
;; (cons 1
;;  (delay (stream-map accum (stream-cdr
;;                     (stream-enumerate-interval 1 20))))

(define y (stream-filter even? seq))
;; sum = 6

;; the promised sequence is seq is now:
;; 1, 3, 6, ...
;; y is
;; (cons-stream
;;  6
;;  (stream-filter even? (stream-cdr
;;                        (stream-map
;;                         accum (stream-enumerate-interval 3 20)))))

;; in seq and y, the first element is always 1/6.

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) seq))
;; now the promised seq is:
;; 1, 8, 11, 15, ...
;; `stream-filter' returns a stream when (stream-car seq) is 15
;; so the sum is 15

(stream-ref y 7)
;; now promised sequence would be
;; 6 24 30 54 64 100 114 162 180
;; remember, the sequence now starts with 6 and grows with interval from 4 to 20
;; => 162, sum is 162

(display-stream z)
;; the promised seq would be
;; 15, 167, 173, 180, ...
;; after filtering, the result would be
;; => 15 180 230 305

;; Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))
;; generate a 2^n sequence start from n = 0
(stream-ref s 10)

;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; multiple integers starting from 2
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(stream-ref factorials 5)

;; Exercise 3.55
;; partial-sum(n) = partial-sum(n - 1) + stream(n)
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

(stream-ref (partial-sums integers) 5)

;; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond
            ((< s1car s2car)
             (cons-stream
              s1car
              (merge (stream-cdr s1) s2)))
            ((< s2car s1car)
             (cons-stream
              s2car
              (merge s1 (stream-cdr s2))))
            (else
             (cons-stream
              s1car ;; keep one value and advance in both streams
              (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge
                                 (scale-stream S 5)
                                 (scale-stream S 3)))))

(define (enumerate-int l h)
  (if (> l h)
      '()
      (cons l (enumerate-int (+ l 1) h))))

(map (lambda (n) (stream-ref S n)) (enumerate-int 0 20))

;; Exercise 3.57
;; when the `delay' is cached, further reference to previous elements in a
;; stream is O(1)

;; as fibs(n) is calculated from fibs(n - 1) and fibs(n - 2)
;; so the steps to computer fibs-steps(n) is 1 + fibs-steps(n - 1)
;; fibs-steps(n) = (n - 2) + fibs-steps(2) = (n - 1)

;; when `delay' is not cached, further reference required calculation as well
;; for the nth fibs, there are (n - 2) addition required to build the stream
;; fibs-steps(n) = (n - 1) + fibs-steps(n - 1) + fibs-steps(n - 2), n >= 3

;; now, verify the above examination:

(define add-with-trace
  (let ((count 0))
    (lambda (m . rest)
      (cond ((eq? m 'count) count)
            ((eq? m 'reset) (set! count 0))
            (else
             (set! count (+ 1 count))
             (apply + (cons m rest)))))))

(define (add-streams s1 s2) (stream-map add-with-trace s1 s2))

(define fibs
  (cons-stream
   0 (cons-stream
      1 (add-streams
         (stream-cdr fibs) fibs))))

;; when `delay' is cached
(stream-ref fibs 9)
(add-with-trace 'count) ;; => 8, correct

;; when `delay' is not cached
(define (fibs-steps n)
  (cond ((= n 0) 0)
        ((= n 1) 0)
        (else
         (+ (- n 1)
            (fibs-steps (- n 1))
            (fibs-steps (- n 2))))))
(equal?
 (map
  (lambda (n)
    (add-with-trace 'reset)
    (stream-ref fibs n)
    (add-with-trace 'count))
  (enumerate-int 0 10))

 (map fibs-steps (enumerate-int 0 10)))

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))

;; (expand 1 7 10)
;; 1, (expand 3 7 10)
;; 1, 4, (expand 2 7 10)
;; 1, 4, 2, (expand 6 7 10),
;; 1, 4, 2, 8, (expand 4 7 10)
;; 1, 4, 2, 8, 5, (epxand 5 7 10)
;; 1, 4, 2, 8, 5, 7, (expand 1 7 10)
;; ....

(map (lambda (x) (stream-ref (expand 1 7 10) x))
     (enumerate-int 0 10))

;; (expand 3 8 10)
;; 3, (expand 6 8 10)
;; 3, 7, (expand 4 8 10)
;; 3, 7, 5, (expand 0 8 10)
;; 3, 7, 5, 0, 0 ....

;; Exercise 3.59
;; a.
(define (integrate series)
  (stream-map / series integers))

(display-stream (integrate (stream-enumerate-interval 1 10)))

;; b.
(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define (neg-stream s)
  (stream-map - s))

(define sine-series
  (cons-stream 0 (integrate cosine-series)))

(define cosine-series
  (cons-stream 1 (neg-stream (integrate sine-series))))

;; Exercise 3.60
(define add-series add-streams)

;;   a0, a1, ... an
;; * b0, b1, ... bn
;; = a0 * (b0 .. bn) + (a1 ... an) * (b0 ... bn)
;; = a0 * b0 + a0 * (b1 ... bn) + (a1 ... an) * (b0 ... bn)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-series
                (scale-stream (stream-cdr s2)
                              (stream-car s1))
                (mul-series (stream-cdr s1)
                            s2))))

;; why this doesn't work, it seems the mul-series cannot print
;; elements longer than its input
;; (display-stream (mul-series (stream-enumerate-interval 1 3)
;;                             (stream-enumerate-interval 1 3)))

(define formula
  (add-streams
   (mul-series sine-series sine-series)
   (mul-series cosine-series cosine-series)))

(map (lambda (n) (stream-ref formula n))
     '(0 1 2 3 4 5))

;; Exercise 3.61
(define (invert-unit-series s)
  (cons-stream
   1 (neg-stream (mul-series (stream-cdr s)
                             (invert-unit-series s)))))

(define s (stream-enumerate-interval 1 3))
(map (lambda (n) (stream-ref (mul-series s (invert-unit-series s)) n))
     '(0 1 2 3 4 5))

;; Exercise 3.62
(define (div-series ns ds)
  (let ((dscar (stream-car ds)))
    (if (zero? dscar)
        (error "denominator has zero constant" (stream-car ds))
        (mul-series ns
                    (scale-stream ;; recover the stream scale
                     (invert-unit-series
                      (scale-stream ds (/ 1 dscar))) ;; invert-unit requires unit constant
                     dscar)))))

(define tangent-series
  (div-series sine-series cosine-series))

;; tan^2 + 1 = 1 / cos^2
(define tan-square
  (mul-series tangent-series tangent-series))

;; tests
(map (lambda (n) (stream-ref
                  (add-series tan-square
                              (neg-stream (invert-unit-series (mul-series cosine-series cosine-series)))
                              ) n))
     '(0 1 2 3 4 5 6 7 8 9 10))
