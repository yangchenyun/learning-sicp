;;;;EVDATA.SCM
            ;;; Evaluator data structures for Chapter 5

;;; Please ignore the following magic for the Scheme compiler.
(declare (usual-integrations))

(define (true? x) (not (eq? x #f)))

(define (false? x) (eq? x #f))

(define the-unspecified-value (list 'the-unspecified-value))

;;; Primitive procedures are inherited from Scheme.

(define primitive-procedure? procedure?)
(define apply-primitive-procedure apply)


;;; Compound procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters p) (list-ref p 1))
(define (procedure-body p) (list-ref p 2))
(define (procedure-environment p) (list-ref p 3))

;;; procedures with declarations are used with the
;;;  cbn interpreter.

(define (make-procedure-with-declarations vars bproc env)
  (list 'procedure-with-declarations vars bproc env))

(define (procedure-with-declarations? obj)
  (tagged-list? obj
		'procedure-with-declarations))


;;;; THE INITIAL ENVIRONMENT

;;; This switch controls whether we give the evaluator access to all
;;; Scheme variables, as in exercise 4.11, versus giving just a fixed
;;; list of primitives.  Giving access to all Scheme variables is a
;;; little dangerous, since Scheme's primitive higher-order procedures
;;; (e.g., MAP) won't work when passed procedures from the MC
;;; evaluator.  For class work, we'll stay with a fixed set of
;;; procedures, and use the other option for experimenting.

(define access-entire-scheme-environment? false)

;;;          Environments
;;; An ENVIRONMENT is a chain of FRAMES.

(define the-empty-environment '())

(define (environment-parent env)
  (cdr env))

(define (first-frame env) (car env))

(define (environment-variables env)
  (car (first-frame env)))

(define (environment-values env)
  (cdr (first-frame env)))

(define (extend-environment variables values base-environment)
  (if (= (length variables) (length values))
      (cons (cons variables values) base-environment)
      (if (< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values))))

(define (make-initial-environment)
  (if access-entire-scheme-environment?
      (make-initial-environment-access-all-scheme)
      (make-initial-environment-restricted)))

(define (make-initial-environment-access-all-scheme)
  (set! scheme-variable-cache '())
  (extend-environment '() '() the-empty-environment))

(define (make-initial-environment-restricted)
  (extend-environment
   primitives-from-underlying-scheme
   (map (lambda (name) (eval name user-initial-environment))
	primitives-from-underlying-scheme)
   the-empty-environment))

(define primitives-from-underlying-scheme
  '(+ - * / inc dec = < > zero? not true false
    cons car cdr pair? null? list symbol? eq? write-line))

(define the-global-environment (make-initial-environment))

(define (lookup-variable-value var env)
  (define (parent-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (parent-loop (cdr env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(lookup-scheme-value var)
	(scan (caar env) (cdar env))))
  (parent-loop env))

(define (set-variable-value! var val env)
  (define (parent-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (parent-loop (cdr env)))
	    ((eq? var (car vars))
	     (set-car! vals val)
	     the-unspecified-value)
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(scan (caar env) (cdar env))))
  (parent-loop env))

(define (define-variable! var val env)
  (define (scan vars vals)
    (cond ((null? vars)
           (set-car! (car env) (cons var (environment-variables env)))
           (set-cdr! (car env) (cons val (environment-values env)))
           the-unspecified-value)
          ((eq? var (car vars))
           (set-car! vals val)
           the-unspecified-value)
          (else
           (scan (cdr vars) (cdr vals)))))
  (scan (environment-variables env)
        (environment-values env)))


;;; We speed up Scheme variable lookup by keeping
;;; a cache of the variables that we actually look up.

(define lexical-unreferenceable?
  (make-primitive-procedure 'lexical-unreferenceable?))

(define lexical-reference
  (make-primitive-procedure 'lexical-reference))

(define scheme-variable-cache '())

(define (lookup-scheme-value var)
  (if access-entire-scheme-environment?
      (let ((vcell (assq var scheme-variable-cache)))
	(cond (vcell (cdr vcell))
	      ((not (lexical-unreferenceable? user-initial-environment var))
	       (let ((val (lexical-reference user-initial-environment var)))
		 (set! scheme-variable-cache
		       (cons (cons var val) scheme-variable-cache))
		 val))
	      (else
	       (error "Unbound variable" var))))
      (error "Unbound variable" var)))


;;; For eceval and compiler in chapter 5
(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

;;; Compiled procedure data structures.  For compiler in section 5.3 

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr  c-proc))
(define (compiled-procedure-env   c-proc) (caddr c-proc))


;;;This is to keep the Scheme printer from going into an infinite loop
;;;if you try to print a circular data structure, such as an environment

(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)

;;;This keeps ECEVAL from printing environments

(define (user-print object)
  (cond ((compound-procedure? object)
         (write (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>)))
        ((compiled-procedure? object)
         (write '<compiled-procedure>))
        (else (write object))))
