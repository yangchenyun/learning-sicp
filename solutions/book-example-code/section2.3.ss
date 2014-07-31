#lang r5rs

;;; Symbolic Differentiation

;; This is my solution to the problem
(define (deriv exp var)
  (define (element? exp) (or (number? exp) (symbol? exp)))
  (define (eq-var? symbol) (eq? var symbol))
  (define (sum-exp? exp) (eq? '+ (car exp)))
  (define (mul-exp? exp) (eq? '* (car exp)))
  (define (sum a b) (list '+ a b))
  (define (mul a b) (list '* a b))
  (define (op1 exp) (cadr exp))
  (define (op2 exp) (caddr exp))
  (define (eval-exp exp)
    (cond ((element? exp) exp)
          ((sum-exp? exp) (eval-sum (op1 exp) (op2 exp)))
          ((mul-exp? exp) (eval-mul (op1 exp) (op2 exp)))))

  (define (eval-sum a b)
    (let ((x (eval-exp a))
          (y (eval-exp b)))
      (cond ((and (number? x) (number? y))
             (+ x y))
            ((and (number? x) (= x 0)) y)
            ((and (number? y) (= y 0)) x)
            (else (sum x y)))))

  (define (eval-mul a b)
    (let ((x (eval-exp a))
          (y (eval-exp b)))
      (cond ((and (number? x) (number? y))
             (* x y))
            ((and (number? x) (= x 1)) y)
            ((and (number? y) (= y 1)) x)
            ((and (number? x) (= x 0)) 0)
            ((and (number? y) (= y 0)) 0)
            (else (mul x y)))))

  (let ((result-exp (cond
          ((element? exp)
           (if (eq-var? exp) 1 0))
          ((sum-exp? exp)
           (sum (deriv (op1 exp) var) (deriv (op2 exp) var)))
          ((mul-exp? exp)
           (let ((a (op1 exp))
                 (b (op2 exp)))
             (sum (mul a (deriv b var)) (mul b (deriv a var)))))
          )))
    (eval-exp result-exp)))

(deriv '(+ x 1) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* (* x y) (+ 1 0))
;;    (* (+ (* x 0) (* 1 y))
;;       (+ x 3)))

;; comparison to the book's solution
;; - share the same recursive procedure
;; - shared the same procedures to perform primitive differentiation algorithm
;; but it has:
;; - better naming of functions
;; - has an error handling function
;; - a separation of implementation of procedures, the primitive procedures
;;   has a very clear group of concepts (variable?, addend, augend etc.)
;; - [!] use constructor to perform the work to simplify the form instead of evaluating it

(define (deriv exp var)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2)) (else (list '* m1 m2))))
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))

  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier s) (cadr s))
  (define (multiplicand s) (caddr s))

  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;; Representing Set

;; 1st attempt, set as unordered list
(define (element-of-set? el set)
  (cond ((null? set) #f)
      ((equal? el (car set)) #t)
      (else (element-of-set? el (cdr set)))))

(define (adjoin-set el set)
  (if (element-of-set? el set)
      set
      (cons el set)))

(define (union-set s1 s2)
  (cond
   ((null? s1) s2)
   ((element-of-set? (car s1) s2)
    (union-set (cdr s1) s2))
   (else (cons (car s1)
               (union-set (cdr s1) s2)))))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define (make-set l) l)

;; Test Cases
(let ((s (make-set '(1 2 3 4))))
  (map (lambda (x)
         (element-of-set? x (adjoin-set x s)))
       '(2 5 a)))

(let ((s (make-set '(1 2 3 4)))
      (t (make-set '(a b c d (1)))))
  (map (lambda (x)
         (equal? (element-of-set? x (union-set t s))
                 (or (element-of-set? x s)
                     (element-of-set? x t))))
       '(2 a '() (1))))

(map (lambda (x)
       (element-of-set? x '()))
     '(2 a '()))

;; 2nd attempt, define set as an ordered list

;; O(n), but in average 1/2 of the original version
(define (element-of-set? el set)
  (cond
   ((null? set) #f)
   ((= el (car set)) #t)
   ((< el (car set)) #f)
   (else
    (element-of-set? el (cdr set)))))

;; O(n)
(define (adjoin-set el set)
  (cond
   ((null? set) (list el))
   ((= el (car set)) set)
   ((< el (car set)) (cons el set))
   ((> el (car set)) (cons (car set)
                           (adjoin-set el (cdr set))))))

;; try to take advantage of the ordered list data structure
;; O(m + n)
(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr s1) (cdr s2))))
              ((< x1 x2)
               (intersection-set (cdr s1) s2))
              ((> x1 x2)
               (intersection-set s1 (cdr s2)))))))

(intersection-set '(1 2 5 8) '(2 4 6 8))

;; try to take advantage of the ordered list as well
;; O(m + n)
(define (union-set s1 s2)
  (if (null? s1)
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (union-set (cdr s1) (cdr s2))))
              ((< x1 x2)
               (cons x1 (union-set (cdr s1) s2)))
              ((> x1 x2)
               (cons x2 (union-set s1 (cdr s2))))))))

(union-set '(1 2 5 8) '(2 4 6 8))

;; 3rd attempt, use the binary search tree form

;; tree is presented by '(entry (left-branch) (right-branch))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; O(logn)
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((= (entry set) x) #t)
   ((> x (entry set)) (element-of-set? x (right-branch set)))
   ((< x (entry set)) (element-of-set? x (left-branch set)))))

;; O(logn)
(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((= (entry set) x) set)
   ((> x (entry set))
    (make-tree (entry set)
               (left-branch set)
               (adjoin-set x (right-branch set))))
   ((< x (entry set))
    (make-tree (entry set)
               (adjoin-set x (left-branch set))
               (right-branch set)))))

(adjoin-set 8 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

;; Huffman Encoding Trees
;; checkout Exercises 2.67 - 2.72
