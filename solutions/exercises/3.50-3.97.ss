#lang racket
(require r5rs)
(load "../lib/stream.ss")
(load "../lib/arithmetic.ss")

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
     '(0 1 2))

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

;; Exercise 3.63
;; draw the environment diagram will help understand this issue
(define (sqrt-improve guess x)
  (average guess (/ guess x)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess)
                   (sqrt-improve guess x)) guesses)))
  guesses)

;; for the textbook's version, a stream named`guesses' returned whose
;; delayed part will be cached.
(stream-ref (sqrt-stream 2) 10)
;; because a local variable `guesses' is present where the whole stream is saved
;; when fetch the 10th element, the 9th element will have already been cached
;; so only one `sqrt-iter' call is required for each step and
;; the time complexity is O(n)

;; with Louis's version
(define (sqrt-stream x)
  (cons-stream
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))

;; as in the textbook's version the computation of nth element depends on
;; the (n-1)th element. Howeverthe stream (sqrt-stream x) is not saved.
;; in the environment, `sqrt-stream' is still a function and (sqrt-stream x)
;; is just an intermediate procedure application which return a stream to be used

;; so each step will involved n `sqrt-improve' call. So the time complexity is O(n^2)

;; if the cache implementation is not used, the `guesses' won't remember its previous
;; value then the textbook's version is O(n^2) as well.

;; Exercise 3.64
(define (stream-limit s tolerance)
  (let ((first (stream-ref s 0))
        (second (stream-ref s 1)))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerence)
  (stream-limit (sqrt-stream x) tolerence))

(sqrt 2 0.00001)

;; Exercise 3.65
(define natrual-log-two-stream
  (partial-sums
   (mul-streams alts (stream-map / float-ones integers))))

;; ln2=0.69314718055994530941...
(display-stream-first natrual-log-two-stream 20)
;; converges slowly within range [0.67, 0.72]

;; use an accelerator, converges at 8th with 0.6931471805604039
(display-stream-first
 (accelerated-sequence euler-transform natrual-log-two-stream) 10)

(define (natrual-log-two tolerance)
  (stream-limit
   (accelerated-sequence euler-transform natrual-log-two-stream) tolerance))

(natrual-log-two 0.000000001)

(display-stream-first (pairs integers integers) 50)

;; Exercise 3.66
(define (stream-count-until pair stream)
  (define (iter count s)
    (if (equal? pair (stream-car s))
        count
        (iter (+ 1 count) (stream-cdr s))))
  (iter 0 stream))

;; pairs are placed by the order of sum of two integers in a pair

;; except (1, 1), (1, n) pair and the rest are interleaving
;; there would be 2 * (n - 1) - 1 pair preceding pair (1, n)

;; (1, 100) has 197 preceding pairs

(= 197 (stream-count-until '(1 100) (pairs integers integers)))

;; now let solve a more general case for (m, n)

;; when m = n, the pairs preceding (m, n) is:
;; m = 1, 1
;; m = 2, 2
;; m = 3, C(m) = 2 * C(m - 1) + 2
;; C(m) = 2^n - 2

;; when n < m, let consider the simple case for (2, n)
;; first of all, considering only pair starting with 2 and larger integer
;; would be 2 * (n - 1) - 3 pairs excluding (2, n) itself according to the above conclusion
;; calculate all pairs starting with integer 1 now.
;; as the interleaving pick pairs from the (1, *) and rest, from the second pair
;; there would be (2n - 5) + 1 pairs interleaved with all the preceding pairs above
;; at last, there would be one pair starting from 1 preceding (2, n) itself
;; so the total pairs preceding (2, n) is (2n - 5) + (2n - 5) + 1 + 1 = 4n - 8 (n > 2)

;; accordingly, (3, n) is
;; pairs start from 3 or larger int: 2 * ((n - 2) - 1) - 1 = 2n - 7
;; pairs start from 2 = 2n - 7 + 1 + 1 = 2n - 5
;; pairs start from 1 = (4n - 12) + 1 + 1 = 4n - 10
;; so the total is 8n - 22 (n > 3)

;; now we found the pattern, let C(m, n) be the count of pairs preceding (m, n)
;; let Cx(m, n) be the count of pairs starting from int larger than x preceding (m, n)
;; Cm(m, n) = 2 * (n - m) - 1 = 2n - (2m + 1)
;; Cm-1(m, n) = C(m, n) + Cm-1(m, n) = 4n - 4m
;; Cm-2(m, n) = C(m - 1, n) + Cm-2(m, n)= 8n - 8m + 2
;; ...
;; C1(m, n) = 2 * C2(m, n) + 2

;; C1(m, n) = 2^m * (n - m) + 2^(m - 1) - 2

;; (99, 100) will be C1(99, 100) = 2^99 + 2^98 - 2
;; (100, 100) will be 2^100 - 2

;; because (99, 100) is too large to test, let's test out for (9, 10) and (10, 10)
;; (9, 10) will have 2^9 + 2^8 - 2 pairs, 766 pairs
;; (10, 10) will have 2^10 - 2, 1022 pairs

(= 766 (stream-count-until '(9 10) (pairs integers integers)))
(= 1022 (stream-count-until '(10 10) (pairs integers integers)))

;; Exercise 3.67
(define (full-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x)) ;; first row
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) ;; first column
                 (stream-cdr s)))
    (full-pairs (stream-cdr s) (stream-cdr t)))))

(display-stream-first (full-pairs integers integers) 20)

;; Exercise 3.68
(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (louis-pairs (stream-cdr s) ;; a direct recursive
                (stream-cdr t))))

;; it doesn't work, will result in an infinite loop
;; when evaluating louis-pairs, it will recursively call
;; `louis-pairs' when evaluating its body and result in
;; an infinite loop

;; for the textbook's version, the recursive process call
;; is delayed through `cons-stream'

;; Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                (stream-cdr (pairs t u))) ;; all pairs i <= j, except (1, 1)
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(display-stream-first (triples integers integers integers) 20)

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (let ((a (car triple))
                         (b (cadr triple))
                         (c (caddr triple)))
                     (= (square c) (+ (square a) (square b)))))
                 (triples integers integers integers)))

(display-stream-first pythagorean-triples 4)

;; Exercise 3.70
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond
            ((< (weight s1car) (weight s2car) 0)
             (cons-stream
              s1car
              (merge-weighted weight (stream-cdr s1) s2)))
            ((> (weight s1car) (weight s2car) 0)
             (cons-stream
              s2car
              (merge-weighted weight s1 (stream-cdr s2))))
            (else
             (cons-stream
              s1car ;; pick the one appears first
              (merge-weighted weight (stream-cdr s1) s2))))))))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

;; a.
(define (sum-weight p)
  (+ (car p) (cadr p)))

(display-stream-first (weighted-pairs sum-weight integers integers) 20)

;; b.
(define (divisible? x y) (= 0 (remainder x y)))
(display-stream-first (weighted-pairs
                       (lambda (p) (+ (* 2 (car p))
                                      (* 3 (cadr p))
                                      (* 5 (car p) (cadr p)))) ;; 2i + 3j + 5ij
                       (stream-filter (lambda (int)
                                        (not (or (divisible? int 2)
                                                 (divisible? int 3)
                                                 (divisible? int 5)
                                                 ))) integers)
                       (stream-filter (lambda (int)
                                        (not (or (divisible? int 2)
                                                 (divisible? int 3)
                                                 (divisible? int 5)
                                                 ))) integers)) 20)

;; Exercise 3.71
(define (cube-sum-weight p)
  (+ (cube (car p))
     (cube (cadr p))))

(define cube-sum-weighted-pairs
  (weighted-pairs cube-sum-weight integers integers))

(define (find-consecutive-equal-weighted weight stream)
  (let ((first (stream-ref stream 0))
        (second (stream-ref stream 1)))
    (if (= (weight first) (weight second))
        (cons-stream (list (weight first) first second)
                     (find-consecutive-equal-weighted weight
                                                      (stream-cdr
                                                       (stream-cdr stream))))
        (find-consecutive-equal-weighted weight (stream-cdr stream)))))

(define ramanujan-numbers
  (find-consecutive-equal-weighted cube-sum-weight cube-sum-weighted-pairs))

(display-stream-first ramanujan-numbers 10)

;; Exercise 3.72

(define (find-consecutive-three-equal-weighted weight stream)
  (let ((first (stream-ref stream 0))
        (second (stream-ref stream 1))
        (third (stream-ref stream 2)))
    (if (= (weight first) (weight second) (weight third))
        (cons-stream (list (weight first) first second third)
                     (find-consecutive-three-equal-weighted
                      weight
                      (stream-cdr
                       (stream-cdr
                        (stream-cdr stream)))))
        (find-consecutive-three-equal-weighted weight (stream-cdr stream)))))

(define (square-sum p)
  (+ (square (car p))
     (square (cadr p))))

(define square-sum-weighted-pairs
  (weighted-pairs square-sum integers integers))

(display-stream-first (find-consecutive-three-equal-weighted square-sum square-sum-weighted-pairs) 10)

;; Exercise 3.73
(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1 C))
               v0 dt))))

(define RC1 (RC 5 1 0.5))

;; Exercise 3.74

(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list)
                   (list->stream (cdr list)))))

(define sense-data
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (sign-change-detector next curr)
  (if (>= (* next curr) 0)
      0 ;; not crossing the zero
      (if (> next curr) 1 -1)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings sense-data 0))

(display-stream-first zero-crossings 12)

;; the generalized solution
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

;; Exercise 3.75
;; Louis's version
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (average (stream-car input-stream)
                       last-value)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossings
      (stream-cdr input-stream) avpt)))) ;; the bug line

;; according to Alyssa, the constructed stream is calculated by
;; the average value of sense-data and its previous value
;; However, in Louis's implementation, the average is calculated by
;; the current value and the 'calculated average' of the previous value

;; The correct implementation
(define (make-zero-crossings input-stream last-avpt last-value)
  (let ((avpt (average (stream-car input-stream)
                       last-value)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings
      (stream-cdr input-stream) avpt (stream-car input-stream)))))

;; Exercise 3.76
(define (smooth s)
  (stream-map average
              s
              (cons-stream (stream-car s) s)))

(define smoothed-zero-crossings
  (let ((smoothed-sense-data (smooth sense-data)))
    (stream-map sign-change-detector
                smoothed-sense-data
                (cons-stream 0 smoothed-sense-data))))

;; Exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral
          (stream-cdr integrand)
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))

;; Exercise 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
               (scale-stream dy a)
               (scale-stream y b)))
  y)

;; Exercise 3.79
(define (solve-2nd-general f y0 dy0 dt)
  (define y (integral (delayed dy) y0 dt))
  (define dy (integral (delayed ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; Exercise 3.80
;; dvc/dt = iC/C = - iL/C
;; diL/dt = vL/L = (vC - vR) / L = (vC - iR * R) = (vC - iL * R)

(define (RLC R L C dt)
  (define (rcl vC0 iL0)
    (define iL (integral (delay diL) iL0 dt))
    (define diL (add-streams
                 (scale-stream iL (- (/ R L)))
                 (scale-stream vC (/ 1 L))))
    (define vC (integral (delay dvC) vC0 dt))
    (define dvC (scale-stream iL (- (/ 1 C))))
    (stream-map list il vC))
  rcl)

(define RLC1 (RLC 1 1 0.2 0.1))
(RLC1 0 10)
