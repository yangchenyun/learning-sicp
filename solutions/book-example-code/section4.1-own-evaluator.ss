#lang r5rs
(load "../lib/dispatch.ss")

;; (define apply-with-underlying-system apply)
(define (tagged-with? list symbol)
  (eq? symbol (car list)))

;; expression abstraction
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))
;; special form shouldn't be treated as variable
(define (variable? exp)
  (and (symbol? exp)
       (not (memq exp *keyword*))))
(define (quote? exp)
  (tagged-with? exp 'quote))
(define text-of-quote cadr)

(define (definition? exp)
  (tagged-with? exp 'define))

(define (def-variable exp)
  (if (pair? (cadr exp))
      (caadr exp) ;; syntax with sugar
      (cadr exp)))

(define (def-body exp)
  (if (pair? (cadr exp))
      (make-lambda (cdadr exp) (seq->exp (cddr exp)))
      (caddr exp)))

(define (assignment? exp)
  (tagged-with? exp 'set!))
(define assignment-variable cadr)
(define assignment-body caddr)

(define (if? exp)
  (tagged-with? exp 'if))
(define if-predicate cadr)
(define if-consequence caddr)
(define (if-alternative exp)
  (if (not (null? cdddr))
      (cadddr exp)
      #f))
(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (lambda? exp)
  (tagged-with? exp 'lambda))
(define lambda-formals cadr)
(define lambda-body cddr)
(define (make-lambda formals body)
  (list 'lambda formals body))

(define (begin? exp)
  (tagged-with? exp 'begin))
(define (make-begin exps)
  (attach-tag 'begin exps))
(define (seq->exp seqs)
  (cond
   ((null? seqs) '())
   ((last-exp? seqs) (first-exp seqs))
   (else (make-begin seqs))))

(define begin-actions cdr)
(define (last-exp? exps)
  (null? (rest-exps exps)))
(define rest-exps cdr)
(define first-exp car)

(define application? pair?)
(define operator car)
(define operands cdr)

;; macro pre-processor
(define (macro? exp)
  (let ((type (car exp)))
    (memq type *build-in-macro*)))
(define *build-in-macro* '())

;; first defined in a case styles
(define (expand exp)
  (let ((expand-proc (get 'expand (car exp))))
    (if expand-proc
        (expand-proc exp)
        (error "unknown macro: EXPAND" exp))))

(define (install-macro-cond)
  (define cond-clauses cdr)
  (define cond-clause-predicate car)
  (define cond-clause-actions cdr)
  (define (cond-else-clause? exp)
    (tagged-with? exp 'else))

  (define (expand exp)
    (define (expand-clauses clauses)
      (if (null? clauses)
          #f
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (seq->exp (cond-clause-actions first))
                    (error "else isn't the last clause" clauses))
                (make-if
                 (cond-clause-predicate first)
                 (seq->exp (cond-clause-actions first))
                 (expand-clauses rest))
                ))))
    (expand-clauses (cond-clauses exp)))

  (set! *build-in-macro*
        (cons 'cond *build-in-macro*))
  (put 'expand 'cond expand))

(install-macro-cond)

(define (eval exp env)
  (cond
   ((self-evaluating? exp)
    exp)
   ((variable? exp)
    (lookup-variable exp env))
   ((quote? exp)
    (text-of-quote exp))
   ((assignment? exp)
    (eval-assignment exp env))
   ((definition? exp)
    (eval-definition exp env))
   ((if? exp)
    (eval-if exp env))
   ;; expand macro before application
   ((macro? exp)
    (eval (expand exp) env))
   ((lambda? exp)
    (make-procedure
     (lambda-formals exp)
     (lambda-body exp)
     env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ((application? exp)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type: EVAL" exp))))

;; special form evaluation
(define (eval-assignment exp env)
  (let ((var (assignment-variable exp))
        (body (assignment-body exp)))
    (update-variable var (eval body env) env)))

(define (eval-definition exp env)
  (let ((var (def-variable exp))
        (body (def-body exp)))
    (add-variable var (eval body env) env)))

(define (eval-if exp env)
  (let ((test (eval (if-predicate exp) env)))
    (if test
        (eval (if-consequence exp) env)
        (eval (if-alternative exp) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (eval (car exps) env)
            (list-of-values (cdr exps) env))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-env
                         (procedure-formals procedure)
                         arguments
                         (procedure-env procedure))))))

(define (apply-primitive procedure arguments)
  (apply-with-underlying-system (primitive-procedure procedure) arguments))

;; procedure is a type-tagged list consists of three components:
;; - formals, body and the environment it is defined
(define (make-procedure formals body env)
  (attach-tag 'procedure
              (list formals body env)))
(define (procedure-formals proc)
  (car (contents proc)))
(define (procedure-body proc)
  (cadr (contents proc)))
(define (procedure-env proc)
  (caddr (contents proc)))
(define (compound-procedure? procedure)
  (eq? 'procedure (type-tag procedure)))

(define (make-primitive-procedure procedure)
  (attach-tag 'primitive procedure))
(define (primitive-procedure procedure)
  (contents procedure))
(define (primitive-procedure? procedure)
  (eq? 'primitive (type-tag procedure)))


;; environment is a pair containing the parent env and a table
;; parent-env | hash-table
(define (make-new-env parent)
  (cons parent (make-hash)))
(define parent-env car)
(define env-table cdr)
(define (set-symbol symbol content env)
  (hash-set! (env-table env) symbol content))
(define (search-symbol symbol env)
  (hash-ref (env-table env) symbol 'not-found))
(define (set-symbols list-of-symbols list-of-contents env)
  (if (not (or (null? list-of-symbols)
               (null? list-of-contents)))
      (begin
        (set-symbol (car list-of-symbols)
                    (car list-of-contents)
                    env)
        (set-symbols (cdr list-of-symbols)
                     (cdr list-of-contents)
                     env))
      'done))

;; operations on the environment hierarchy
(define *global-env* (make-new-env '()))
(define (global-env? env) (null? (car env)))
(define (reset-global-env)
  (set! *global-env* (make-new-env '())))
(define *build-in-procedures*
  '(+ - * / % cons car cdr null? = > < >= <=))
(define *keyword*
  '(if quote define set! lambda begin))
(define (setup-env env)
  (set-symbols *build-in-procedures*
               (map make-primitive-procedure
                    (list + - * / modulo cons car cdr null? = > < >= <=))
               env)
  (set-symbol 'true #t env)
  (set-symbol 'false #f env))

(define (lookup-variable symbol env)
  (if (null? env)
      (error "variable not found: LOOKUP-VARIABLE" symbol)
      (let ((content (search-symbol symbol env)))
        (if (eq? content 'not-found)
            (lookup-variable symbol (parent-env env))
            content))))

(define add-variable set-symbol)

(define (update-variable symbol new-content env)
  (if (null? env)
      (error "variable doesn't exist: UPDATE-VARIABLE" symbol)
      (let ((content (search-symbol symbol env)))
        (if (eq? content 'not-found)
            (update-variable symbol new-content (parent-env env))
            (set-symbol symbol new-content env)))))

(define (extend-env formals arguments env)
  (let ((new-env (make-new-env env)))
    ;; assume formals and arguments have same length
    (if (not (= (length formals)
                (length arguments)))
        (error "formals and arguments have diff length: EXTEND-ENV"
               (list formals arguments))
        (set-symbols formals arguments new-env))
    new-env))

(define (run prog)
  (let ((global-env (make-new-env '())))
    (setup-env global-env)
    (eval-sequence prog global-env)))

;; tests
(define (assert name res expect)
  (if (not (equal? res expect))
      (error "Failed test:" name (list res expect))
      'ok))

;; primitive expression
(assert 'number-test
        (run '(2)) 2)
(assert 'string-test
        (run '("3")) "3")
(assert 'quote-test
        (run '((quote (+ 1 2))))
        '(+ 1 2))

(assert 'primitive-procedure
        (run '((+ 1 2)))
        3)

(assert 'definition
        (run '((define x 1)
               (define y 2)
               (+ x y)))
        3)

;; calling lambda
(assert 'simple-lambda
        (run '((define add
                 (lambda (u w) (+ w u)))
               (add 1 2)))
        3)

(assert 'define-syntax-sugar
        (run '((define (identity a) a)
               (identity 2)))
        2)

(assert 'if-true
        (run '((if true 'true 'false)))
        'true)

(assert 'if-false
        (run '((if false 'true 'false)))
        'false)

(assert 'cond-two-clauses
        (run '((cond
                (true 'true)
                (false 'false)))) 'true)

(assert 'cond-multiple-clauses
        (run '((cond
                ((= 1 3) 1)
                ((= 2 3) 2)
                ((= 3 3) 3)))) 3)

(assert 'cond-else-clauses
        (run '((cond
                ((= 1 3) 1)
                ((= 2 3) 2)
                (else 3)))) 3)

(assert 'anonymous-conditional-lambda
        (run '(((if false
                    (lambda (x) (+ x 1))
                    (lambda (x) (+ x 2)))
                5)))
        7)

(assert 'recursive-procedure
        (run '((define (factorial n)
                 (if (= n 1)
                     1
                     (* n (factorial (- n 1)))))
               (factorial 10)))
        3628800)

(assert 'nested-defines
        (run '((define (fib-i n)
                 (define (fib-iter a b count)
                   (if (= count n)
                       b
                       (fib-iter
                        b
                        (+ a b)
                        (+ count 1))))
                 (fib-iter 0 1 1))
               (fib-i 10)))
        '55)

(assert 'map-procedure
        (run '((define (map proc list)
                 (if (null? list)
                     '()
                     (cons
                      (proc (car list))
                      (map proc (cdr list)))))

               (map (lambda (x) (+ x 1))
                    '(1 2 3))))
        '(2 3 4))
