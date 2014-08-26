#lang racket
;; (require r5rs)
(require rackunit)
;; (load "../lib/dispatch.ss")

;; Exercise 4.1
;; evaluate from left to right
(define (list-of-values exps env)
  (if (null? exps)
      '()
      (let ((first-value (eval (car exps) env)))
        (cons first-value (list-of-values (cdr exps) env)))))

(list-of-values '((displayln 1)
                  (displayln 2)
                  (displayln 3)) (current-namespace))

;; evaluate from right to left

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (let ((rest-values (list-of-values (cdr exps) env)))
        (cons (eval (car exps) env) rest-values))))

(list-of-values '((displayln 1)
                  (displayln 2)
                  (displayln 3)) (current-namespace))

;; Exercise 4.2
;; if application is evaluated before assignments, an expression like (define x 3)
;; will be evaluated as a normal application with a procedure named by `define'

;; the syntax for the application needed to be adjusted
(define (application? exp) (tagged-with? 'exp 'call))
(define operator cadr)
(define operands cddr)

;; Exercise 4.3
;; use data-directed style
(define (install-number-eval)
  (define (number-eval exp env)
    exp)
  (put 'eval 'number number-eval))

(define (install-string-eval)
  (define (string-eval exp env)
    exp)
  (put 'eval 'string string-eval))

(define (install-symbol-eval)
  (define (symbol-eval exp env)
    (lookup-variable exp env))
  (put 'eval 'symbol symbol-eval))

(define (install-quote-eval)
  (define (quote-eval exp env)
    (text-of-quote exp))
  (put 'eval 'quote quote-eval))

(define (install-assignment-eval)
  (define (assign-eval exp env)
    (let ((var (assignment-variable exp))
          (body (assignment-body exp)))
      (update-variable var (eval body env) env)))
  (put 'eval 'set! assign-eval))

(define (install-define-eval)
  (define (define-eval exp env)
    (let ((var (def-variable exp))
          (body (def-body exp)))
      (add-variable var (eval body env) env)))
  (put 'eval 'define define-eval))

(define (install-if-eval)
  (define (if-eval exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequence exp) env)
        (eval (if-alternative exp) env)))
  (put 'eval 'if if-eval))

(define (install-lambda-eval)
  (define (lambda-eval exp env)
    (make-procedure
     (lambda-formals exp)
     (lambda-body exp)
     env))
  (put 'eval 'lambda lambda-eval))

(define (install-begin-eval)
  (define (begin-eval exp env)
    (eval-sequence (begin-actions exp) env))
  (put 'eval 'begin begin-eval))

(install-number-eval)
(install-string-eval)
(install-symbol-eval)
(install-quote-eval)
(install-assignment-eval)
(install-define-eval)
(install-begin-eval)
(install-if-eval)

(define (exp-type exp)
  (cond
   ((number? exp) 'number)
   ((string? exp) 'string)
   ((symbol? exp) 'symbol)
   (else (car exp))))

(define (eval exp env)
  (let ((type (exp-type exp))) ;; number and variable should type-tagged
    (let ((proc (get 'eval type)))
      (if proc
          (proc exp env)
          (if (application? exp)
              (apply (eval (operator exp) env)
                     (map (lambda (operand)
                            (eval operand env)) (operands exp)))
              (error "Unknown type to eval: EVAL" (list type exp)))))))

;; Compare with data-directed style, the tag used to detect the expression type
;; is reserved after dispatch.

;; Exercise 4.4
;; install syntax
(define (or? exp)
  (tagged-with? exp 'or))
(define or-clauses cdr)

(define (and? exp)
  (tagged-with? exp 'and))
(define and-clauses cdr)

;; added eval-function to eval
;; (define (eval exp env)
;;   ...
;;    ((or? exp)
;;     (eval-or exp env))
;;    ((and? exp)
;;     (eval-and exp env))
;;   ...
;; )

;; special form evaluation
(define (eval-or exp env)
  (define (eval-or-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first-value (eval (car clauses) env))
              (rest (cdr clauses)))
          (if (true? first-value)
              first-value
              (eval-or-clauses rest)))))

  (eval-or-clauses (or-clauses exp)))

(define (eval-and exp env)
  (define (eval-and-clauses clauses)
    (cond ((null? clauses) 'true)
          ((last-exp? clauses) (eval (first-exp clauses) env))
          (else
           (let ((first-value (eval (car clauses) env))
                 (rest (cdr clauses)))
             (if (not (true? first-value))
                 'false
                 (eval-and-clauses rest))))))
  (eval-and-clauses (and-clauses exp)))

;; they could also be implemented as derived expression
;; (or <exp1> <exp2> ... <expn>)

;; could be transform to
;; (if <exp1>
;;     <exp1>
;;     (if <exp2>
;;         <exp2>
;;         ...
;;         (if <expn>
;;             <expn>
;;             'false)
;;         ))

;; define as a macro package

(define (install-macro-or)
  (define or-clauses cdr)

  (define (expand exp)
    (define (expand-clauses clauses)
      (if (null? clauses)
          'false
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (make-if first
                     first
                     (expand-clauses rest)))))
    (expand-clauses (or-clauses exp)))

  (set! *build-in-macro*
        (cons 'or *build-in-macro*))
  (put 'expand 'or expand))

;; (and <exp1> <exp2> ... <expn>)

;; could be transform to
;; (if <exp1>
;;     (if <exp2>
;;         ...
;;         (if <expn>
;;             <expn>
;;             'false)
;;         'false)
;;     'false)

(define (install-macro-and)
  (define (expand exp)
    (define (expand-clauses clauses)
      (cond ((null? clauses) 'true)
            ((last-exp? clauses)
             (make-if (first-exp clauses)
                      (first-exp clauses)
                      'false))
            (else (let ((first (car clauses))
                        (rest (cdr clauses)))
                    (make-if first
                             (expand-clauses rest)
                             'false)))))
    (expand-clauses (and-clauses exp)))

  (set! *build-in-macro*
        (cons 'and *build-in-macro*))
  (put 'expand 'and expand))

;; Exercise 4.5
(define (install-macro-cond)
  (define cond-clauses cdr)
  (define cond-clause-predicate car)
  (define cond-clause-actions cdr)
  (define (cond-else-clause? exp)
    (tagged-with? exp 'else))
  (define (cond-recipient-clause? exp)
    (eq? '=> (cadr exp)))
  (define (cond-clause-recipient exp)
    (caddr exp))

  (define (expand exp)
    (define (expand-clauses clauses)
      (if (null? clauses)
          'false
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (seq->exp (cond-clause-actions first))
                    (error "else isn't the last clause" clauses))
                (make-if
                 (cond-clause-predicate first)
                 (if (cond-recipient-clause? first)
                     (cons (cond-clause-recipient first)
                           (list (cond-clause-predicate first)))
                     (seq->exp (cond-clause-actions first)))
                 (expand-clauses rest))
                ))))
    (expand-clauses (cond-clauses exp)))

  (set! *build-in-macro*
        (cons 'cond *build-in-macro*))
  (put 'expand 'cond expand))

;; Exercise 4.6
(define (install-macro-let)
  (define let-declare-clauses cadr)
  (define let-body-clauses cddr)
  (define (expand exp)
    (let ((vars (map car (let-declare-clauses exp)))
          (exps (map cadr (let-declare-clauses exp))))
      (cons (make-lambda vars (seq->exp (let-body-clauses exp))) exps)))

  (set! *build-in-macro*
        (cons 'let *build-in-macro*))
  (put 'expand 'let expand))

;; Exercise 4.7
;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))

;; could be transformed to

;; (let ((x 3))
;;   (let ((y (+ x 2)))
;;     (let ((z (+ x y 5)))
;;       (* x z))))

;; it is sufficient to handle let* as derived expression

(define (install-macro-let*)
  (define let*-declare-clauses cadr)
  (define let*-body-clauses cddr)
  (define (make-let declare-clauses body-clauses)
    (append (list 'let declare-clauses) (list body-clauses)))

  (define (build-nested-lets declares body)
    (if (null? declares)
        (seq->exp body)
        (make-let (list (car declares))
                  (build-nested-lets (cdr declares) body))))

  (define (expand exp)
    (build-nested-lets
     (let*-declare-clauses exp)
     (let*-body-clauses exp)))

  (set! *build-in-macro*
        (cons 'let* *build-in-macro*))
  (put 'expand 'let* expand))


;; Exercise 4.8
(define (install-macro-let)
  (define (make-let declare-clauses body-clauses)
    (append (list 'let declare-clauses) body-clauses))
  (define (named-let? exp)
    (not (pair? (cadr exp))))

  (define (expand-normal-let exp)
    (define let-declare-clauses cadr)
    (define let-body-clauses cddr)
    (let ((vars (map car (let-declare-clauses exp)))
          (exps (map cadr (let-declare-clauses exp))))
      (cons (make-lambda vars (seq->exp (let-body-clauses exp))) exps)))

  (define (expand-named-let exp)
    (define let-var cadr)
    (define let-declare-clauses caddr)
    (define let-body-clauses cdddr)

    (make-let (let-declare-clauses exp)
              (append
               (list (make-definition (let-var exp)
                                      (make-lambda (map car (let-declare-clauses exp))
                                                   (seq->exp (let-body-clauses exp)))))
               (let-body-clauses exp))))

  (define (expand exp)
    (if (named-let? exp)
        (expand-named-let exp)
        (expand-normal-let exp)))

  (set! *build-in-macro*
        (cons 'let *build-in-macro*))
  (put 'expand 'let expand))

;; Exercise 4.9
;; definition of do, from r5rs

;; (do ((<var1> <init1> <step1>)
;;      ...)
;;     (<test> <expression> ...)
;;     <command> ... )
(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i))

;; could be expanded as
(let ((vec (make-vector 5))
      (i 0))
  (define (iter vec i)
    (if (= i 5)
        vec
        (begin
          (vector-set! vec i i)
          (set! i (+ i 1))
          (iter vec i))))
  (iter vec i))

(define (install-macro-do)
  (define do-declares cadr)
  (define (do-vars exp)
    (map car (do-declares exp)))
  (define (do-inits exp)
    (map cadr (do-declares exp)))

  (define (do-steps exp)
    (map (lambda (declare)
           (if (null? (cddr declare))
               (car declare) ;; keep the same value
               (caddr declare))) (do-declares exp)))
  (define do-test caaddr)
  (define do-result-exps cdaddr)
  (define do-commands cdddr)

  (define (make-let-declares exp)
    (map (lambda (declare)
           (cons (car declare)
                 (list (cadr declare)))) (do-declares exp)))

  (define (make-let declares body)
    (append (list 'let declares)
            body))

  (define (expand exp)
    (define iter-call
      (cons 'iter (do-vars exp)))

    (define iter-body
      ;; evaluate commands
      (append (seq->exp (do-commands exp))
              (list (make-let
                     ;; evaluate steps
                     (map (lambda (var step)
                            (list var step))
                          (do-vars exp)
                          (do-steps exp))
                     ;; binding and execute the iter in new environment
                     (list iter-call)))))

    (define iter-definition
      (make-definition 'iter
                       (make-lambda (do-vars exp)
                                    (make-if
                                     (do-test exp)
                                     (seq->exp (do-result-exps exp))
                                     (seq->exp iter-body)
                                     ))))

    (make-let (make-let-declares exp) (list iter-definition iter-call)))

  (set! *build-in-macro*
        (cons 'do *build-in-macro*))
  (put 'expand 'do expand))

;; Exercise 4.10
;; an abstraction layer is built to extract information the expression
;; to define a new syntax, only the expression constructor and selector
;; are needed for changes

;; for example, we could change the keyword for definition

(define (definition? exp)
  (tagged-with? exp 'var))
(define (make-definition var exp)
  (list 'var var exp))

;; now (var a 1) will define a new variable in the environment

;; furthermore, we could even use a normal order application such as:
;; (1 + 2) instead of (+ 1 2)
;; (1 > 2) instead of (> 1 2)

;; what we need to change in only the selector for the application syntax
(define operator cadr)
(define (operands exp)
  (cons (car exp) (cddr exp)))

;; For Exercise 4.11 - 4.13

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame!
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Exercise 4.11

;; changes to the frame data structure

;; use a headed-list to unify the operations
(define (make-frame variables values)
  (cons 'table
        (map cons variables values)))
(define (frame-pairs frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (frame-pairs frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (if (null? bindings)
          (env-loop
           (enclosing-environment env))
          (let ((first-binding (car bindings)))
            (if (eq? var (car first-binding))
                (cdr first-binding)
                (scan (cdr bindings))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-pairs frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (if (null? bindings)
          (env-loop
           (enclosing-environment env))
          (let ((first-binding (car bindings)))
            (if (eq? var (car first-binding))
                (set-cdr! first-binding val)
                (scan (cdr bindings))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-pairs frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (if (null? bindings)
          (add-binding-to-frame! var val frame)
          (let ((first-binding (car bindings)))
            (if (eq? var (car first-binding))
                (set-cdr! first-binding val)
                (scan (cdr bindings))))))
    (scan (frame-pairs frame))))

(test-eq? "lookup variables in env"
          (let ((global-env
                 (extend-environment '(a) '(1) the-empty-environment)))
            (lookup-variable-value 'a global-env))
          1)

(test-eq? "define variables in env"
          (let ((global-env
                 (extend-environment '() '() the-empty-environment)))
            (define-variable! 'a 1 global-env)
            (lookup-variable-value 'a global-env))
          1)

(test-eq? "set variables in env"
          (let ((global-env
                 (extend-environment '(a) '(1) the-empty-environment)))
            (set-variable-value! 'a 2 global-env)
            (lookup-variable-value 'a global-env))
          2)

(test-exn "set non-exist variables"
          exn:fail?
          (lambda ()
            (let ((global-env
                   (extend-environment '() '() the-empty-environment)))
              (define-variable! 'a 1 global-env)
              (set-variable-value! 'b 2 global-env))))

(test-eq? "lookup var in enclosing env"
          (let* ((global-env
                  (extend-environment '(a) '(1) the-empty-environment))
                 (internal-env
                  (extend-environment '() '() global-env)))
            (lookup-variable-value 'a internal-env))
          1)

(test-eq? "set var in enclosing env"
          (let* ((global-env
                  (extend-environment '(a) '(1) the-empty-environment))
                 (internal-env
                  (extend-environment '() '() global-env)))
            (set-variable-value! 'a 2 internal-env)
            (lookup-variable-value 'a internal-env))
          2)

(test-exn "lookup non-exist variable"
          exn:fail?
          (lambda ()
            (let ((global-env
                   (extend-environment '(a) '(1) the-empty-environment)))
              (lookup-variable-value 'b global-env))))

;; Exercise 4.12

;; use original data structure
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; a general traversal procedure

;; a data abstraction for the return data type
;; use compound data to avoid conflicts with primitive data types
(define (make-return-result type result)
  (cons type result))
(define return-type car)
(define return-result cdr)
(define null-return (make-return-result 'null '()))

(define (traverse-env env frame-proc end-proc)
  (if (eq? the-empty-environment env)
      (end-proc)
      (let ((frame (first-frame env)))
        (let ((return (frame-proc frame)))
          (if (eq? 'done (return-type return))
              (return-result return)
              (traverse-env (enclosing-environment env)
                            frame-proc
                            end-proc))))))

(define (make-frame-processer binding-proc end-frame-proc)
  (lambda (frame)
    (define (scan vars values)
      (if (null? vars)
          (begin
            (end-frame-proc frame))
          (let ((return (binding-proc vars values)))
            (if (eq? 'done (return-type return))
                return
                (scan (cdr vars)
                      (cdr values))))))
    (let ((vars (frame-variables frame))
          (values (frame-values frame)))
      (scan vars values))))

(define (lookup-variable-value var env)
  (define (lookup-proc vars vals)
    (if (eq? var (car vars))
        (make-return-result 'done (car vals))
        null-return))
  (traverse-env
   env
   (make-frame-processer lookup-proc
                         (lambda (frame)
                           null-return))
   (lambda ()
     (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (define (set!-proc vars vals)
    (if (eq? var (car vars))
        (begin
          (set-car! vals val)
          (make-return-result 'done val))
        null-return))
  (traverse-env
   env
   (make-frame-processer set!-proc
                         (lambda (frame)
                           null-return))
   (lambda ()
     (error "Unbound variable: SET!" var))))

(define (define-variable! var val env)
  (define (define!-proc vars vals)
    (if (eq? var (car vars))
        (begin
          (set-car! vals val)
          (make-return-result 'done val))
        null-return))

  (define (end-each-frame-proc frame)
    (begin
      (add-binding-to-frame! var val frame)
      (make-return-result 'done val)))

  (traverse-env env
                (make-frame-processer
                 define!-proc
                 end-each-frame-proc)
                void))

;; Exercise 4.13
;; make-unbound! should remove the binding in the first frame of environment
;; it should mimic the specification of define

(define (unbound-variable! var env)
  (define (unbound!-proc vars vals)
    (if (eq? var (car vars))
        (begin
          (set-car! vars 'removed) ;; use a special symbol for removed var
          (make-return-result 'done var))
        null-return))

  (define (end-each-frame-proc frame)
    (make-return-result 'done '()))

  (traverse-env env
                (make-frame-processer
                 unbound!-proc
                 end-each-frame-proc)
                void))

(test-exn "unbound variable in one environment"
          exn:fail?
          (lambda ()
            (let ((global-env
                   (extend-environment '(a) '(1) the-empty-environment)))
              (unbound-variable! 'a global-env)
              (lookup-variable-value 'a global-env))))

(test-eq? "unbound variable in enclosing environment"
          (let* ((global-env
                  (extend-environment '(a) '(1) the-empty-environment))
                 (internal-env
                  (extend-environment '() '() global-env)))
            (unbound-variable! 'a internal-env)
            (lookup-variable-value 'a internal-env)) 1)

;; Exercise 4.14

;; For Eva's version, `map' is defined within the implemented language
;; to evaluate a procedure with `map', for example (map (lambda (x) x) '(1 2 3))
;; the eval-apply model will reduce it to primitive procedures with in the
;; underlying Scheme system

;; However with Louis's version, `map' is defined as a primitive
;; so a call to (map (lambda (x) x) '(1 2 3)), will actually pass the operands
;; *in the implemented* language to the underlying Scheme system. The operands
;; are simple lists '(lambda (x) x) and '(1 2 3), obviously this breaks the `map'
;; calls
