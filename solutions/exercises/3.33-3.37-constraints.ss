#lang r5rs
(require r5rs)
(load "../book-example-code/section3.3.5-constraints.ss")

;; Exercise 3.33
;; 2c = a + b
(define (averager a b c)
  (let ((w (make-connector))
        (x (make-connector)))
    (multiplier x c w)
    (adder a b w)
    (constant 2 x)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "a" a)
(probe "b" b)
(probe "c" c)
(averager a b c)

(set-value! a 10 'user)
(set-value! b 30 'user)

(forget-value! b 'user)
(set-value! c 40 'user)

;; Exercise 3.34
;; on one hand, the change of a is sufficient to trigger a change in b
;; and produce the correct result
(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)

(squarer a b)

(set-value! a 10 'user)

;; however, setting only b won't be able to trigger an update in a,
;; because internally, `squarer' relies on `multiplier' which requires
;; at least two value to trigger an update

(forget-value! a 'user)
(set-value! b 100 'user)

;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (let ((bv (get-value b)))
          (if (< bv 0)
              (error "square less than 0: SQUARER" bv)
              (set-value! a (sqrt bv) self)))
        (let ((av (get-value a)))
          (set-value! b (* av av) self))))

  (define (process-forget-value)
    (forget-value! a)
    (forget-value! b)
    (process-new-value))

  (define (self m)
    (cond
     ((eq? m 'new-value) (process-new-value))
     ((eq? m 'no-value) (process-forget-value))
     (else (error "Unknown request: SQUARER" m))))
  (connect a self)
  (connect b self)
  self)

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)

(squarer a b)

(set-value! b 100 'user)

;; Exercise 3.36

;; Exercise 3.37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* m n)
  (let ((p (make-connector)))
    (multiplier m n p)
    p))

(define (c/ a b)
  (let ((c (make-connector)))
    (multiplier b c a)
    c))

(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(set-value! C 25 'user)
(get-value F)
