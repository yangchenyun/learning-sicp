;;; This is the code for ps1.

;;; Part 1

(- 8 9) ;; -1

(> 3.7 4.4) ;; #f

(- (if (> 3 4)
       7
       10)
   (/ 16 10)) ;; 42/5

(define b 13) ;; undef

13 ;; 13

b ;; 13

> ;; procedure representation of >

(define square (lambda (x) (* x x))) ;; undef


square ;; procedure represents square

(square 13) ;; 169

(square b) ;; 169

(square (square (/ b 1.3))) ;; 10000

(define multiple-by-itself square) ;; undef

(multiple-by-itself b) ;; 169

(define a b)

(= a b) ;; #t

(if (= (* a b) (square 13))
    (< a b)
    (- a b)) ;; #f

(cond ((>= a 2) b)
      ((< (square b) (multiple-by-itself a)) (/ 1 0))
      (else (abs (- (square a) b)))) ;; 13

;;; Part 2 Use the Debugger

(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a) (p2 b))))

;;; Exercise

;;; Exercise 1.1
;;; No problem is detected.

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
          (spindle x)))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

;;; Exercise 1.2

;;; version with problem
(define fact
  (lambda (n)
    (if (= n 0)
        (* n (fact (- n 1))))))

;;; correct version
(define fact
  (lambda (n)
    (cond ((< n 0) (error "fact could only apply to none-negative integer"))
          ((= n 0) 1)
          (else (if (= n 1)
                    1
                    (* (fact (- n 1)) n))))))

;;; Exercise 1.3
(define comb
  (lambda (n k)
    (/ (fact n)
       (*
        (fact k)
        (fact (- n k))))))


;;; Exercise 1.11

(define foo1
  (lambda (x)
    (* x x)))
;; (foo1 (sqrt 3))

(define foo2
  (lambda (x y)
    (/ x y)))
;; (foo2 6 2)

(define foo3
  (lambda (x)
    (lambda (y)
      (/ x y))))
;; ((foo3 6) 2)
;; (foo3 6) is an curried application of foo3

(define foo4
  (lambda (x)
    (x 3)))

;; (foo4 (lambda (x) (* 1 x)))

(define foo5
  (lambda (x)
    (cond ((= x 2)
           (lambda () x))
          (else
           (lambda () (* x 3))))))
;; ((foo5 1))

;; FIXME
(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))

;; (foo6 (lambda(a) (+ 1 a)))
;; ((lambda (a) (+ 1 a)) (lambda (y) (y y)))
;; the parameter passed to foo6 should be able to alter a procedure
