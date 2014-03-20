;;;;SMSYNTAX.SCM   Code for Sample Pset
;;; Syntax for Substitution Model (Includes DESUGAR)

                ;;IGNORE THESE DECLARATIONS--THEY'RE FOR THE COMPILER

(declare (usual-integrations))
(declare (integrate-operator tagged-pair?))
(declare (integrate-operator make-define))
(declare (integrate-operator define?))
(declare (integrate-operator variable-of-define))
(declare (integrate-operator expression-of-define))
(declare (integrate-operator expression?))
(declare (integrate-operator make-body))
(declare (integrate-operator defines-of-body))
(declare (integrate-operator expression-of-body))
(declare (integrate-operator self-evaluating?))
(declare (integrate-operator symbol-expression?))
(declare (integrate-operator make-symbol))
(declare (integrate-operator make-combination))
(declare (integrate-operator operator-of))
(declare (integrate-operator operands-of))
(declare (integrate-operator make-lambda))
(declare (integrate-operator lambda-expression?))
(declare (integrate-operator formals-of-lambda))
(declare (integrate-operator body-of-lambda))
(declare (integrate-operator make-if))
(declare (integrate-operator if?))
(declare (integrate-operator test-of-if))
(declare (integrate-operator consequent-of))
(declare (integrate-operator alternative-of))
(declare (integrate-operator variable?))
(declare (integrate-operator primitive-procedure-variable?))
(declare (integrate-operator primitive-procedure-variables))
(declare (integrate-operator rule-specified-procedure-variable?))
(declare (integrate-operator keyword?))
(declare (integrate-operator expression-keyword?))
;(declare (integrate-operator immediate-value?))
(declare (integrate-operator false-value?))
(declare (integrate-operator submodel-nil))
(declare (integrate-operator submodel-null?))
(declare (integrate-operator submodel-useless-value))
(declare (integrate-operator submodel-useless-value?))
(declare (integrate-operator tag-of))
(declare (integrate-operator make-tagged-body))
(declare (integrate-operator datum-of-tagged-body))
(declare (integrate-operator make-step))
(declare (integrate-operator step-number-of))
(declare (integrate-operator body-or-info-of-step))
(declare (integrate-operator make-tagged-steps))
(declare (integrate-operator datum-of-tagged-steps))
(declare (integrate-operator make-tagged-exps))
(declare (integrate-operator datum-of-tagged-exps))
(declare (integrate-operator expressions-of-exps-datum))
(declare (integrate-operator defines-of-exps-datum))
(declare (integrate-operator make-tagged-defs))
(declare (integrate-operator defines-of-defs-datum))
(declare (integrate-operator expression-keywords))
(declare (integrate-operator nonexpression-keywords))
(declare (integrate-operator keywords))
(declare (integrate-operator cond?))
(declare (integrate-operator clauses-of-cond))
(declare (integrate-operator test-of-clause))
(declare (integrate-operator expression-of-clause))
(declare (integrate-operator else-clause?))
(declare (integrate-operator make-cond))
(declare (integrate-operator let?))
(declare (integrate-operator bindings-of-let))
(declare (integrate-operator variable-of-binding))
(declare (integrate-operator init-of-binding))
(declare (integrate-operator body-of-let))


;;TAG TEST

(define (tagged-pair? tag)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) tag))))


;;DEFINE

(define (make-define var exp)
  (list 'define var exp))

(define define? (tagged-pair? 'define))

(define (variable-of-define def)
  ;;handles both (define f exp) and (define (f x1 ...) body)
  (let ((var-form (cadr def)))
    (if (variable? var-form)
        var-form
        (car var-form))))
      
(define (expression-of-define def)
  ;;handles both (define f exp) and (define (f x1 ...) body)
  (let ((var-form (cadr def))) 
    (if (variable? var-form)
        (caddr def)
        (make-lambda
         (cdr var-form)               ;formals
         (let ((body-parts (cddr def)))
           (if (define? (car body-parts))
               body-parts
               (car body-parts))))))) ;BODY-PARTS must be length 1 list of <expression>

;;EXPRESSION

(define (expression? body)
  (or (self-evaluating? body)
      (variable? body)
      (special-form? body)
      (symbol-expression? body)       ;redundant in our representation
      (submodel-null? body)           ;redundant in our representation
      (combination? body)))

(define (special-form? body)
  (and (pair? body) (expression-keyword? (car body))))

;;BODY

(define (make-body defines exp)
 (if (null? defines)
     exp
     (append defines (list exp))))

(define (defines-of-body body)
  (define (all-but-last l)
    (reverse (cdr (reverse l))))
  (if (expression? body)
      '()
      (all-but-last body)))

(define (expression-of-body body)
  (define (last l)
    (car (reverse l)))
  (if (expression? body)
      body
      (last body)))

;;SELF-EVALUATING

(define (self-evaluating? body)
  (or (boolean? body)
      (number? body)))

;;SYMBOL

(define (symbol-expression? body)
  (and ((tagged-pair? 'quote) body)
       (pair? (cdr body))
       (symbol? (cadr body))
       (null? (cddr body))))

(define (make-symbol name)
  (list 'quote name))

(define symbol-name cadr)

;;COMBINATION

(define make-combination cons)

(define (combination? body)
  (and (pair? body)
       (list? body)
       (for-all? body expression?)))

(define operator-of car)
(define operands-of cdr)

;;LAMBDA-EXPRESSION

(define (make-lambda formals body)
  (cons 'lambda
        (cons formals
              (if (expression? body)
                  (list body)
                  body))))
(define lambda-expression? (tagged-pair? 'lambda))
(define formals-of-lambda cadr)
(define (body-of-lambda lam-exp)
  (let ((bodylist (cddr lam-exp)))
    (if (null? (cdr bodylist))          ;more efficient than (= (length bodylist) 1)
        (car bodylist)
        bodylist)))                 

;;IF

(define (make-if test consequent alternative)
  (list 'if test consequent alternative))

(define if? (tagged-pair? 'if))
  
(define test-of-if cadr)
(define consequent-of caddr)
(define alternative-of cadddr)

;;VARIABLE

(define (variable? body)
  (and (symbol? body)
       (not (keyword? body))))

(define (primitive-procedure-variable? body)
  (and (symbol? body)
       (memq body primitive-procedure-variables)))

(define primitive-procedure-variables
  '(+ - * / = < > >= <= inc dec 1+ -1+ 
    quotient sqrt expt round abs gcd
    max min remainder modulo zero? 
    positive? negative? odd? even?
    exp log sin cos tan asin acos atan
    number? not boolean? symbol? eq?
    procedure? length))

(define (rule-specified-procedure-variable? var)
  (and (symbol? var)
       (assq var rule-association-list)))


;;KEYWORD

(define expression-keywords		;COND, LET treated as sugar
  '(quote lambda if let cond))

(define nonexpression-keywords '(define else))

(define keywords (append nonexpression-keywords expression-keywords))

(define (keyword? tag)
  (and (symbol? tag)
       (memq tag keywords)))

(define (expression-keyword? tag)
  (and (symbol? tag)
       (memq tag expression-keywords)))

;;VALUE

(define (immediate-value? body)
  (or (self-evaluating? body)
      (submodel-null? body)
      (lambda-expression? body)
      (symbol-expression? body)
      (and (combination? body)
           (eq? 'cons (operator-of body))
           (let ((rands (operands-of body)))
             (and (immediate-value? (car rands))
                  (immediate-value? (cadr rands)))))
      (primitive-procedure-variable? body)
      (rule-specified-procedure-variable? body)))


(define false-value? not)

(define submodel-nil '())
(define submodel-null? null?)

;(define submodel-nil '(quote ()))

;(define (submodel-null? body)
;  (and
;   ((tagged-pair? 'quote) body)
;   (null? (cadr body))
;   (null? (cddr body))))

;;;in the Sub Model, the USELESS-VALUE will be a symbol

(define submodel-useless-value (make-symbol 'useless-value))
(define (submodel-useless-value? body) (equal? body submodel-useless-value))

;;TAGGED TYPES

;;selector TAG-OF returns the tag of any tagged type
(define tag-of car)

;; Tagged-Body  =  Labelled-Sum[stepped: <body>,
;;                                  val: <body>,
;;                                stuck: Error-info]
;;  Body-Tag = {stepped, val, stuck}

;;constructor MAKE-TAGGED-BODY: (Body-Tag, (<body> U Error-Info)) --> Tagged-Body
;;selector DATUM-OF-TAGGED-BODY: Tagged-Body --> (<body> U Error-Info)

(define make-tagged-body list)
(define datum-of-tagged-body cadr)


;; Tagged-Steps  =  Step-Tag  x  List(Step)
;; Step-Tag = {interrupted, val, stuck}
;; Step = Sch-Positive-Integer x (<body>) U Error-info)
;; constructor MAKE-STEP: (Sch-Positive-Integer, (<body> U Error-Info)) --> Step
;; selectors   STEP-NUMBER-OF: Step --> Sch-Positive-Integer
;;             BODY-OR-INFO-OF-STEP: Step --> (<body> U Error-Info)

(define make-step list)
(define step-number-of car)
(define body-or-info-of-step cadr)


;;MAKE-TAGGED-STEPS: (Tag, List(Step)) --> Tagged-Steps

;;constructor:
(define make-tagged-steps cons)

;;selector:
(define datum-of-tagged-steps cdr)



;; Tagged-Exps  =  Labelled-Sum[stepped: List(<define>) X List(<exp>),
;;                                  val: Empty,
;;                                stuck: Error-info]

(define make-tagged-exps list)
(define datum-of-tagged-exps cdr)
(define expressions-of-exps-datum cadr)
(define defines-of-exps-datum car)


;; Tagged-Defs   =  Labelled-Sum[stepped: List(<define>),
;;                                   val: Empty,
;;                                 stuck: Error-info]

(define make-tagged-defs list)
(define defines-of-defs-datum cadr)



;;;;DESUGAR


;;COND
(define cond? (tagged-pair? 'cond))
(define clauses-of-cond cdr)
(define test-of-clause car)
(define expression-of-clause cadr)
(define else-clause? (tagged-pair? 'else))
(define (make-cond clauses) (cons 'cond clauses))

;;LET

(define let? (tagged-pair? 'let))
(define bindings-of-let cadr)
(define variable-of-binding car)
(define init-of-binding cadr)
(define (body-of-let let-exp)
  (let ((bodylist (cddr let-exp)))
    (if (null? (cdr bodylist))          ;more efficient than (= (length bodylist) 1)
        (car bodylist)
        bodylist)))


;;DESUGAR: <body in extended grammar> --> <body in kernel grammar>

(define (desugar body)
  (cond ((cond? body)
         (let ((clauses (clauses-of-cond body)))
           (if (null? clauses)
               submodel-useless-value
               (let ((first-clause (car clauses)))
                 (desugar
                  (if (else-clause? first-clause)
                      (expression-of-clause first-clause)
                      (make-if
                       (test-of-clause first-clause)
                       (expression-of-clause first-clause)
                       (make-cond (cdr clauses)))))))))
        ((let? body)
         (desugar
          (let ((bindings (bindings-of-let body)))
            (make-combination
             (make-lambda
              (map variable-of-binding bindings)
              (body-of-let body))
             (map init-of-binding bindings)))))
        ((or (self-evaluating? body)
             (symbol-expression? body)
             (variable? body)
             (submodel-null? body))
         body)
        ((lambda-expression? body)
         (make-lambda
          (formals-of-lambda body)
          (desugar (body-of-lambda body))))
        ((combination? body)
         (make-combination
          (desugar (operator-of body))
          (map desugar (operands-of body))))
        ((if? body)
         (make-if
          (desugar (test-of-if body))
          (desugar (consequent-of body))
          (desugar (alternative-of body))))
        (else
         (let ((defs (defines-of-body body)))
           (make-body
            (map make-define
                 (map variable-of-define defs)
                 (map desugar (map expression-of-define defs)))
            (desugar (expression-of-body body)))))))

;;patch for problem set typo

(define (operator x) (operator-of x))

(define (operands x) (operands-of x))
