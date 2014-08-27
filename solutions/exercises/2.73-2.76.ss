#lang r5rs
(#%require racket/include)
(include "../lib/dispatch.ss")
(include "../lib/arithmetic.ss")
(include "../lib/utils.ss")

;; Exercise 2.73
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; treat the operator as a type tag
(define operator car)
(define operands cdr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))
;; a.
;; `deriv' returns for number? and variable? exp, otherwise
;; it dispatches on the type of exp for proper `deriv' methods
;; on that type

;; Because the exp in that case doesn't have the type-tag and
;; cannot be dispatched.
;; But actually we could define another two types, 'var and 'num
;; and make this generic as well

;; b.
(define (install-sums-deriv)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  ;; choose a different name from deriv
  ;; to avoid the clojure scope inside
  (define (exp-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ exp-deriv))

(define (install-products-deriv)
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2)) (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (exp-deriv exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'deriv '* exp-deriv))

(install-sums-deriv)
(install-products-deriv)

;; c.

(define (install-exponents-deriv)
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (make-exponentiation base expo)
    (cond
     ((=number? expo 1) base)
     ((=number? expo 0) 1)
     (else (list '** base expo))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2)) (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (exp-deriv exp var)
    (make-product (exponent exp)
                  (make-product
                   (make-exponentiation
                    (base exp)
                    (make-sum (exponent exp) -1))
                   (deriv (base exp) var))))

  (put 'deriv '** exp-deriv))

(install-exponents-deriv)
(deriv '(** (+ (** x 2) 2) 3) 'x)

;; d.
;; the install method in each package `put' will need to be changed

;; Exercise 2.74
;; personnel records, contains a set of records keyed on employees' name
;; employee record,

;; a.
(define (get-record personnel employee)
  (apply-generic 'get-record personnel employee))

;; there is no restriction on the main data structure for the personnel file
;; as long as it contains the division name in the head. In particular, it
;; must provide information to identify the division
;; In addition, the implementation also requires the employee record through the `get-record' method

;; b.
(define (get-salary personnel employee)
  (let ((record (get-record personnel employee)))
    (apply-generic 'get-salary record)))

;; the employee record data also must provide the division information as well

;; c.
(define (find-employee-record employee personnels)
  (if (null? personnels)
      #f
      (let* ((personel (car personnels))
             (record (get-record personnel employee)))
        (if record
            record
            (find-employee-record employee (cdr personnels))))))

;; d.
;; the new corporate just need to follow the above restriction on
;; personnel file and employee record

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude r))
          ((eq? op 'angle a))
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.76
;; generic operation with explicit dispatch
;; for every new type, all the operation must added one dispatch condition
;; for every new operation, it needed to create a new method for all types

;; data-directed style
;; for every new type
;; - a new package needed to be implemented to include all operations
;; - an install package procedure call

;; for every new operation
;; - all packages needed to implement and install the operation
;; - a global operation procedure

;; message-passing style
;; for every new type
;; - a new constructor embodies all the operations as a dispatch proc

;; for every new operation
;; - all data type needed to be updated to include the new operation

;; Data-directed style is more explicit about the accessible operations
;; in the sytem, so it is suitable for a system with heavy operation changes.
;; Message-passing style exposes the data type as interfaces to the system
;; and is more suitable for systems with heavy type changes.
