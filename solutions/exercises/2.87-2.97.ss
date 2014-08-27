#lang r5rs
(#%require racket/include)
(include "../lib/dispatch.ss")
(include "../lib/arithmetic.ss")

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;; Symbolic Algebra
;; assumed tagging syste
(define (attach-tag type-tag contents)
  (if (or (symbol? contents)
          (number? contents))
      contents
      (cons type-tag contents)))

;; datum -> symbol
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((symbol? datum) 'scheme-symbol)
        ((number? datum) 'scheme-number)
      (error "Bad tagged datum: TYPE-TAG" datum)))

;; datum -> data
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((or (symbol? datum) (number? datum)) datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; primitives: constants | variables
;; combinations: linear, polynominal, rational, trigonometric
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
    (displayln p1)
    (displayln p2)
    (newline)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              MUL-POLY"
               (list p1 p2))))

  (define (poly-=zero? p)
    ;; coefficients in all orders are =zero?
    (foldr (lambda (co zero-before?)
             (and zero-before? (=zero? co))) #t
           (map coeff (term-list p))))

  (define (poly-neg p)
    ;; coefficients are all negged
    (make-poly (variable p)
               (map neg-term (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'neg '(polynomial)
       (lambda (p) (tag (poly-neg p))))
  (put '=zero? '(polynomial) poly-=zero?)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (poly-neg p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (neg-term term)
  (make-term (order term)
             (neg (coeff term))))
(define (make-term order coeff)
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list)
  (null? term-list))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1
                   (add-terms (rest-terms L1)
                              L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2
                   (add-terms
                    L1
                    (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term
                    (order t1)
                    (add (coeff t1)
                         (coeff t2)))
                   (add-terms
                    (rest-terms L1)
                    (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms
       (mul-term-by-all-terms
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms
          t1
          (rest-terms L))))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Exericise 2.87

(define (install-polynomial-package-287)
  (define (poly-=zero? p)
    ;; coefficients in all orders are =zero?
    (foldr (lambda (co zero-before?)
             (and zero-before? (=zero? co))) #t
             (map coeff (term-list p))))

  ;; interface to rest of the system
  (put '=zero? '(polynomial) poly-=zero?))

(define p3 (make-polynomial 'x
                            (list
                             (make-term 2
                                        (make-polynomial 'y
                                                          '((1 1) (0 1))))
                             (make-term 1
                                        (make-polynomial 'y
                                                          '((2 1) (0 1))))
                             (make-term 0
                                        (make-polynomial 'y
                                                         '((1 1) (0 -1)))))))

(define p4 (make-polynomial 'x
                            (list
                             (make-term 1
                                        (make-polynomial 'y
                                                         '((1 1) (0 -2))))
                             (make-term 0
                                        (make-polynomial 'y
                                                         '((3 1) (0 7)))))))
(add p3 p4)

;; Exercise 2.88
;; define a generic `neg' procedure for all

;; has some problems, rectangular and polar now needs to know complex number
;; in rectangular package
;; (define (neg z)
;;   (make-complex-from-real-imag (- (real-part z))
;;                        (- (imag-part z))))

;; in polar package
;; (define (neg z)
;;   (make-complex-from-mag-ang (magnitude z) (+ pi (angle z))))

;; in complex package
;; (define (complex-neg z) (neg z))

;; in scheme-number package
;; (define neg -)

;; in rational package
;; (define (neg r)
;;   (make-rat (- (numer r)) (denom r)))

(neg (make-rational 2 1))
(neg (make-scheme-number 2))
(neg (make-complex-from-real-imag 3 2))
(neg (make-complex-from-mag-ang 3 .5))

;; neg works on the 2nd level data [<tag>, <tag>, <data>]
;; neg inside each package works on the 1st level data [<tag>, <data>]

;; in poly package
(define (neg-term term)
  (make-term (order term)
             (neg (coeff term))))

(define (install-polynomial-package-288)
  (define (poly-neg p)
    ;; coefficients are all negged
    (make-poly (variable p)
               (map neg-term (term-list p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (poly-neg p2))))))

(sub p3 p4)
