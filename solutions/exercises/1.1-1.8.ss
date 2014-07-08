#lang r5rs
;; Exercise 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ; => (+ 3 4 (* 3 4)) = 19
(= a b) ;#f
(if (and (> b a) (< b (* a b)))
    b
    a) ;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16

(+ 2 (if (> b a) b a)) ;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

;; Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(define (sum-two-large-squares a b c)
  (define (square x) (* x x))
  (define (sum-of-square a b)
    (+ (square a) (square b)))
  (if (> a b)
      (if (> b c)
          (sum-of-square a b)
          (sum-of-square a c))
      (if (> a c)
          (sum-of-square a c)
          (sum-of-square b c))))

;; Exericse 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; It defines a procedure named a-plus-abs-b which takes two arguments
;; When the second argument is larger than >
;; Apply the '+' operator to the two operands
;; Otherwise, apply the '-' operator to the two opearands

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;; In a normal-order procedure, the operands are not
;; evaluated until the value is needed
(test 0 (p))
(if (= 0 0) 0 (p))
;; the (= 0 0) is evaluated and 0 is returned

;; In an application-order, the operands are first evaluated
(test 0 (p))
(test 0 (p))
...
;; In this case (p) results in a infinite evaluation loop

;; Exercise 1.6
(abs x)

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))

;; the new-if is a normal LISP expression which uses
;; an application-order evaluation
;; in thie case, the operands will be evaluated firstly
;; as the new-sqrt-iter evaluates to itself
;; it falls into an infinite loop

;; the build-in if expression uses a special evaluation order

;; Exercise 1.7
(square (sqrt 0.00001)) ; => 0.0000678 which is away from the error
(square (sqrt 1e50)) ; => too long operation time
;; because for each iteration, the calculation (- 1.0 1e50) is too little
;; franction of changes for the operation
;; The limited precision of large numbers disables the effectiveness
;; of the good-enough? comparision

(define (good-enough? guess x)
  (< (/ (abs (-
              guess
              (improve guess x)))
        guess)
     0.00001))
;; the new method compares the marginal changes of each guess
;; and it reaches more precision for small and large numbers

;; Exercise 1.8
(define (abs x)
  (if (< x 0) (- x) x))

(define (cube-sqrt x)
  (define (cube-improve guess)
    (/ (+
        (/ x (square guess))
        (* 2 guess))
       3))

  (define (good-enough? guess)
    (< (/ (abs (-
                guess
                (cube-improve guess)))
          guess)
       0.00001))

  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (cube-improve guess))))

  (cube-iter 1.0))
