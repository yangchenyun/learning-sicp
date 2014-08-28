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
(cons
  (apply show (stream-enumerate-interval 0 10)) ;; this display 0
  (delay
    (apply stream-map show (stream-cdr (stream-enumerate-interval 0 10)))))

(cons 0
  (delay
    (apply stream-map show (stream-cdr (stream-enumerate-interval 0 10)))))

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
