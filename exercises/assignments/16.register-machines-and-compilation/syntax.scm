;;;; Syntax procedures for chapter 4

(declare (usual-integrations))

;;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp #t)
      (eq? exp #f)
      (string? exp)))	; Our prompt (viz., "M-EVAL=> ") is a string.

;;; Variables

(define (variable? exp) (symbol? exp))

;;; Special forms (in general)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;;; Quotations

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;;; Assignment--- SET!

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;; Definitions

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cons 'lambda
            (cons (cdadr exp)		;formal parameters
                  (cddr exp)))))	;body


;;; LAMBDA expressions
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))

;;; If conditionals

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'if pred conseq alternative))


;;; BEGIN expressions (a.k.a. sequences)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define no-more-exps? null?)


(define (sequence->begin seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin exp) (cons 'begin exp))


;;; Procedure applications

(define (application? exp)
  (pair? exp))

(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (last-operand? args) (null? (cdr args)))	;for chapter 5

;;; COND Conditionals

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses cndl) (cdr cndl))
(define (cond-no-clauses? clauses) (null? clauses))
(define (cond-first-clause clauses) (car clauses))
(define (cond-rest-clauses clauses) (cdr clauses))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if cond-exp)
  (define (expand clauses)
    (if (cond-no-clauses? clauses)
        'the-unspecified-value)
        (let ((first (cond-first-clause clauses))
              (rest (cond-rest-clauses clauses)))
          (if (cond-else-clause? first)
              (if (cond-no-clauses? rest)
                  (sequence->begin (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                         exp))
              (make-if (cond-predicate first)
                       (sequence->begin (cond-actions first))
                       (expand rest)))))
  (expand (cond-clauses cond-exp)))

;;; LET expressions

(define (let? exp) (tagged-list? exp 'let))

(define (let-bound-variables let-exp)
  (map car (cadr let-exp)))

(define (let-values let-exp) (map cadr (cadr let-exp)))

(define (let-body let-exp) (sequence->begin (cddr let-exp)))

(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
	(values (let-values let-exp))
	(body (let-body let-exp)))
    (cons (list 'LAMBDA names body)
	  values)))


;;; Declaration syntax for call-by-name interpreter

(define declaration? pair?)

(define (parameter-name var-decl)
  (if (pair? var-decl)
      (car var-decl)
      var-decl))

(define (lazy? var-decl)
  (and (pair? var-decl)
       (eq? 'lazy (cadr var-decl))))

(define (memo? var-decl)
  (and (pair? var-decl)
       (eq? 'lazy-memo (cadr var-decl))))

(define (exists p? l)
  (let loop ((l l))
    (cond ((null? l) false)
	  ((p? (car l)) true)
	  (else (loop (cdr l))))))


;;; Other special forms that are used in AMBScheme.

(define (amb? exp)
  (and (pair? exp)
       (eq? (car exp) 'amb)))

(define (if-can? exp)
  (and (pair? exp)
       (eq? (car exp) 'if-can)))
