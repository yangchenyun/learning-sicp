#lang r5rs
(require r5rs)

(define (call-each procs)
  (if (null? procs)
      'done
      (begin
        ((car procs))
        (call-each (cdr procs)))))

;; 3.3.5 Constraints
;; concepts:
;; primitive constraintts: certain relationship holds between quantities
;; connectors: holds a value which may participate in more constraints
;; constraint networks: a network of connectors and constraints

;; as the wire before connect holds an value
;; and list of constraints it is connected to
(define (make-connector)
  (let ((value #f)
        (resolved #f)
        (action-procs '()))

    (define (accept-action! proc)
      (set! action-procs
            (cons proc action-procs)))

    (define (set-value! new-value)
      (cond ((not (has-value? self))
             (begin
               (set! value new-value)
               (set! resolved #t)
               (call-each action-procs)))
            ((not (= new-value value))
             (error "Contradiction" (list value new-value)))
            (else 'ignored)))

    (define (forget-value!)
      (begin
        (set! value #f)
        (set! resolved #f)
        'done))

    (define (self m)
      (cond
       ((eq? m 'get-value) value)
       ((eq? m 'add-action!) accept-action!)
       ((eq? m 'set-value!) set-value!)
       ((eq? m 'forget-value!) (forget-value!))
       ((eq? m 'has-value?) resolved)
       (else (error "Unknown method for connector" m))))
    self))

;; helper methods
(define (get-value connector)
  (connector 'get-value))
(define (forget-value! connector)
  (connector 'forget-value!))
(define (has-value? connector)
  (connector 'has-value?))
(define (set-value! connector new-value)
  ((connector 'set-value!) new-value))
(define (add-action! connector action)
  ((connector 'add-action!) action))

;; define constraints, it must accept the 'wakeup message and
;; perform "proper" operation to fulfill the constraints
;; if any value is updated, it should be

(define (resolved-count connectors)
  (define (iter l c)
    (if (null? l)
        c
        (iter (cdr l)
              (+ c (if (car l) 1 0)))))
  (iter (map has-value? connectors) 0))

;; add a listener to all other-connectors
;; apply the value of other-connectors to the proc
;; when and only when all the other-connectors are fulfilled
(define (enforce-relation connector proc . others)
  (define (apply-when-resolved)
    (let ((resolved-res
           (if (= (length others)
                  (resolved-count others))
               (apply proc (map get-value others))
               #f)))
      (if resolved-res
          (set-value! connector resolved-res))))
  (for-each (lambda (other)
              (add-action! other apply-when-resolved)) others))

;; the equation should be mapped as dynamic relations
(define (adder a1 a2 sum)
  (enforce-relation a1 (lambda (a2 sum)
                         (- sum a2)) a2 sum)
  (enforce-relation a2 (lambda (a1 sum)
                         (- sum a1)) a1 sum)
  (enforce-relation sum (lambda (a1 a2)
                          (+ a1 a2)) a1 a2))

(define (multiplier m1 m2 product)
  (enforce-relation m1 (lambda (m2 product)
                         (/ product m2)) m2 product)
  (enforce-relation m2 (lambda (m1 product)
                         (/ product m1)) m1 product)
  (enforce-relation product (lambda (m1 m2)
                          (* m1 m2)) m1 m2))

(define (constant value connector)
  (set-value! connector value))

(define (probe-connector name connector)
  (add-action! connector
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display " New-value = ")
                 (display (get-value connector)))))

;; a little tests over constraints
(define a (make-connector))
(define b (make-connector))
(define s (make-connector))
(probe-connector 'a a)
(probe-connector 'b b)
(probe-connector 's s)
(adder a b s)

(set-value! a 3.14)
(set-value! b 0.5)
;; s New-value = 7

(define m (make-connector))
(define n (make-connector))
(define p (make-connector))
(probe-connector 'm m)
(probe-connector 'n n)
(probe-connector 'p p)
(multiplier m n p)

(set-value! m 3.14)
;; m New-value = 3
(set-value! n 4.5)
;; n New-value = 4
;; p New-value = 12

;; now test against our system
(define (celsius-fahrenheit-converter C F)
  (let
      ((w (make-connector))
       (u (make-connector))
       (v (make-connector))
       (x (make-connector))
       (y (make-connector)))

    (multiplier C w u) ;; C*w = u
    (multiplier v x u) ;; v*x = u
    (adder v y F) ;; F + y = v
    (constant 5 x)
    (constant 32 y)
    (constant 9 w)
    'ok))

(define C (make-connector))
(define F (make-connector))
(constant 25 C)
(celsius-fahrenheit-converter C F)

(get-value F) ;; should return the correct results, 77

;; some thoughts with comparison of implementation in the book

;; I tried to abstract a message system between connector and
;; constraint but I found there will be a lot handling for equations
;; and I choose to treat constraint as an enforcer of relationship
;; between connectors and all the relation happened directly between
;; connectors

;; I also use the time scheduling model from last example, but it seems
;; could be avoided at all

;; I also missing the ability to checking the value to be set
;; and the avoid loop notification
