#lang racket
(load "../lib/dispatch.ss")
(load "../lib/assert.ss")

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
  (and (pair? exp)
       (tagged-with? exp 'quote)))

(define text-of-quote cadr)

(define (definition? exp)
  (and (pair? exp)
       (tagged-with? exp 'define)))

(define (def-variable exp)
  (if (pair? (cadr exp))
      (caadr exp) ;; syntax with sugar
      (cadr exp)))

(define (def-body exp)
  (if (pair? (cadr exp))
      (make-lambda (cdadr exp) (seq->exp (cddr exp)))
      (caddr exp)))

(define (make-definition var exp)
  (list 'define var exp))

(define (assignment? exp)
  (and (pair? exp)
       (tagged-with? exp 'set!)))
(define assignment-variable cadr)
(define assignment-body caddr)
(define (make-assignment var body)
  (list 'set! var body))

(define (if? exp)
  (and (pair? exp)
       (tagged-with? exp 'if)))
(define if-predicate cadr)
(define if-consequence caddr)
(define (if-alternative exp)
  (if (not (null? cdddr))
      (cadddr exp)
      #f))
(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (lambda? exp)
  (and (pair? exp)
       (tagged-with? exp 'lambda)))
(define lambda-formals cadr)
(define lambda-body cddr)
(define (make-lambda formals body)
  (list 'lambda formals body))

(define (begin? exp)
  (and (pair? exp)
       (tagged-with? exp 'begin)))
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

(define (install-macro-and)
  (define and-clauses cdr)
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

(define (install-macro-let*)
  (define let*-declare-clauses cadr)
  (define let*-body-clauses cddr)

  (define (build-nested-lets declares body)
    (if (null? declares)
        (seq->exp body)
        (append (list 'let (list (car declares)))
                (list (build-nested-lets (cdr declares) body)))))

  (define (expand exp)
    (build-nested-lets
     (let*-declare-clauses exp)
     (let*-body-clauses exp)))

  (set! *build-in-macro*
        (cons 'let* *build-in-macro*))
  (put 'expand 'let* expand))

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

(install-macro-cond)
(install-macro-or)
(install-macro-and)
(install-macro-let)
(install-macro-let*)
(install-macro-do)

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
           (map (lambda (operand)
                  (eval operand env)) (operands exp))))
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
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequence exp) env)
      (eval (if-alternative exp) env)))

;; choose to use the same truth representation in implementing and implemented
;; language
(define (true? exp)
  exp)

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

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
(define (scan-out-defines body)
  (let ((definitions '()))
    (define (make-let declares body)
      (append (list 'let declares)
              body))

    (define (filter-out-definitions exps)
      (cond
       ((not (pair? exps)) exps)
       ((null? exps) '())
       ((definition? (car exps))
        (begin
          (set! definitions (cons (car exps) definitions))
          (filter-out-definitions (cdr exps))))
       (else
        (cons (car exps) (filter-out-definitions (cdr exps))))))

    (let* ((filtered-body (filter-out-definitions body)))
      (if (null? definitions)
          body
          (let ((def-declares (map (lambda (def)
                                     (list (def-variable def) '*unassigned*))
                                   definitions))
                (def-set-clauses (map (lambda (def)
                                        (make-assignment (def-variable def)
                                                         (def-body def)))
                                      definitions)))
            (make-let def-declares (append def-set-clauses filtered-body))
            )))))

(define (make-procedure formals body env)
  (attach-tag 'procedure
              (list formals (scan-out-defines body) env)))
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
(define (make-global-env)
  (make-new-env '()))
(define env-root? null?)

(define *build-in-procedures*
  '(+ - * / % remainder assoc cons car cdr null? = > < >= <= random))
(define *keyword*
  '(if quote define set! lambda begin))
(define (setup-env env)
  (set-symbols *build-in-procedures*
               (map make-primitive-procedure
                    (list + - * / modulo remainder assoc cons car cdr null? = > < >= <= random))
               env)
  (set-symbol 'true #t env)
  (set-symbol 'false #f env))

(define (lookup-variable symbol env)
  (if (env-root? env)
      (error "variable not found: LOOKUP-VARIABLE" symbol)
      (let ((content (search-symbol symbol env)))
        (cond ((eq? content 'not-found)
               (lookup-variable symbol (parent-env env)))
              ((eq? content '*unassigned*)
               (error "variable is not assigned: LOOKUP-VARIABLE" symbol))
              (else content)))))

(define add-variable set-symbol)

(define (update-variable symbol new-content env)
  (if (env-root? env)
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

(assert 'cond-recipient-clause
        (run '((define (cadr list)
                 (car (cdr list)))
               (cond ((assoc 'b '((a 1) (b 2))) => cadr)
                     (else false)))) 2)

(assert 'or-test-no-clause
        (run '((or))) #f)

(assert 'or-test-middle
        (run '((define a 'init)
               (or ((lambda ()
                      (set! a 'changed)
                      a)) 1 false))) 'changed)

(assert 'or-test-no-unnecessary-eval
        (run '((define a 'init)
               (or 1 ((lambda ()
                        (set! a 'changed))) false)
               a)) 'init)

(assert 'or-test-false
        (run '((or false false false)))
        #f)

(assert 'and-test-no-clause
        (run '((and))) #t)

(assert 'and-test-true
        (run '((and true true))) #t) ;; FIXME: true evals to #t in the implemented lang

(assert 'and-test-false
        (run '((and false true))) #f)

(assert 'and-test-last-exp
        (run '((and true 1))) 1)

(assert 'and-test-no-unnecessary
        (run '((define a 'init)
               (and false ((lambda ()
                             (set! a 'changed))))
               a
               )) 'init)

(assert 'let-definition
        (run '((let ((a 1) (b 2))
                 (+ a b)))) 3)

(assert 'named-let
        (run '((let fib-iter ((a 1) (b 0) (count 10))
                 (if (= count 0)
                     b
                     (fib-iter (+ a b)
                               a
                               (- count 1)))))) 55)

(assert 'let*-macro
        (run '((let* ((x 3)
                      (y (+ x 2))
                      (z (+ x y 5)))
                 (* x z)))) 39)

(assert 'do-iteration
        (run '((let ((x '(1 3 5 7 9)))
                 (do ((x x (cdr x))
                      (sum 0 (+ sum (car x)))) ((null? x) sum))))) 25)

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

(assert 'even-fib
        (run '((define (even? n) (= 0 (remainder n 2)))
               (define (fib n)
                 (define (fib-iter a b count)
                   (if (= count 0)
                       b
                       (fib-iter (+ a b) a (- count 1))))
                 (fib-iter 1 0 n))
               (define (even-fibs n)
                 (define (next k)
                   (if (> k n)
                       '()
                       (let ((f (fib k)))
                         (if (even? f)
                             (cons f (next (+ k 1)))
                             (next (+ k 1))))))
                 (next 0))
               (even-fibs 10))) '(0 2 8 34))

(assert 'fermat-test-prog
        (run
         '((define (square x) (* x x))
           (define (even? n) (= 0 (remainder n 2)))
           (define (map proc list)
             (if (null? list)
                 '()
                 (cons
                  (proc (car list))
                  (map proc (cdr list)))))
           (define (expmod b exp m)
             (cond ((= exp 0) 1)
                   ((even? exp)
                    (remainder
                     (square (expmod b (/ exp 2) m))
                     m))
                   (else
                    (remainder
                     (* b (expmod b (- exp 1) m))
                     m))))

           (define (fermat-test n)
             (define (try-it a)
               (= (expmod a n n) a))
             (try-it (+ 1 (random (- n 1)))))

           (define (fast-prime? n times)
             (cond ((= 0 times) 'true)
                   ((fermat-test n) (fast-prime? n (- times 1)))
                   (else 'false)))
           (map (lambda (x) (fast-prime? x 10))
                '(2 3 4 5 93 561 1105 1729 2465))))
        '(true true false true false true true true true))
