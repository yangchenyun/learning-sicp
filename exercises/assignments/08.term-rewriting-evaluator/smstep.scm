;;;;SMSTEP.SCM   Code for Sample Pset
;;; Routines for One Rewriting Step of Substitution Model 

                              ;;FOR THE COMPILER
(declare (usual-integrations))          
(declare (integrate-external "smsyntax"))
(declare (integrate-external "smscope"))


;; The main procedure of the Scheme Term Rewriting Model implementation
;; is ONE-STEP-BODY, which does a single rewrite step, if possible, on a
;; body.  ONE-STEP-BODY satisfies a simple "contract" which allows it to be
;; used effectively without studying, or even looking at, its definition:

                         ;;;CONTRACT FOR ONE-STEP-BODY

;; ONE-STEP-BODY: (<body>,List(<define>)) --> Tagged-Body
;; Tagged-Body  =  Labelled-Sum[stepped: <body>,
;;                                  val: <body>,
;;                                stuck: Error-info]

;;In a call (ONE-STEP-BODY body defines-from-context):

;;           BODY must be desugared.
;;           If a rewrite rule applies to BODY, using DEFINES-FROM-CONTEXT for
;;           lookup of undefined variables, then the call returns the tagged result
;;                  ('stepped, rewritten body);
;;           if body is a syntactic value, then it returns
;;                  ('val, garbage collected body);
;;           if body cannot be rewritten and is not a value, then it returns
;;                  ('stuck, info on the problem).


(define (one-step-body body defines-from-context)
  (cond ((immediate-value? body)        ;;WARNING: management is not
                                        ;;responsible if a body redefines
                                        ;;any of the primitive procedure variables
                                        ;;or rule-specified procedure variables.
         (make-tagged-body 'val body))
        ((variable? body)
         (one-step-variable body defines-from-context))
        ((combination? body)
         (one-step-combination
          (operator-of body)
          (operands-of body)
          defines-from-context))
        ((if? body)
         (one-step-if (test-of-if body)
                      (consequent-of body)
                      (alternative-of body)
                      defines-from-context))
        (else
         (one-step-body-with-defines
          (defines-of-body body)
          (expression-of-body body)
          defines-from-context))))


(define (one-step-variable var defines-from-context)
  (let ((var-def (lookup-define var defines-from-context)))
    (if (define? var-def)
        (let ((exp (expression-of-define var-def)))
          (if (immediate-value? exp)
              (make-tagged-body 'stepped exp)
              (make-tagged-body 'stuck (list 'unassigned-variable: var-def))))
        (make-tagged-body 'stuck (list 'unbound-variable: var)))))


(define (lookup-define var defines)
  (if (null? defines)
      #f
      (let ((def (car defines)))
        (if (eq? var (variable-of-define def))
            def
            (lookup-define var (cdr defines))))))


(define (one-step-if test consequent alternative defines-from-context)
  (let ((test-result (one-step-body test defines-from-context)))
    (let ((tag (tag-of test-result))
          (body (datum-of-tagged-body test-result)))
      (cond ((eq? tag 'stepped)
             (make-tagged-body
              'stepped
              (make-body (defines-of-body body)
                         (make-if
                          (expression-of-body body)
                          consequent
                          alternative))))
            ((eq? tag 'val)             ;test must have been an <immediate-value>
             (make-tagged-body
              'stepped
              (if (false-value? test)
                  alternative
                  consequent)))
            ((eq? tag 'stuck)
             test-result)))))


;; ONE-STEP-EXPS: (List(<exp>),List(<define>)) --> Tagged-Exps
;; Tagged-Exps  =  Labelled-Sum[stepped: List(<define>) X List(<exp>),
;;                                  val: Empty,
;;                                stuck: Error-info]

;;(ONE-STEP-EXPS exps defines-from-context)
;;does ONE-STEP-BODY on successive expressions in the list EXPS until
;;an expression, M, takes a rewriting step.  Let L be the list of expressions
;;preceding M, followed by the expression part of rewritten M,
;;followed by the rest of exps.  (The expressions preceding M must all
;;have been <immediate value>s.)  Then the result returned is
;;('stepped, ((defines-of rewritten M),L)).
;;If no expression rewrites, then ('val) is returned.

;;We're using the fact that if ONE-STEP-BODY returns with tag 'val after
;;being applied to an <expression> (as opposed to a <body> with nonnull <define>s),
;;then the expression must have been an immediate value in the first place,
;;so there's no new expression to return.

(define (one-step-exps exps defines-from-context)
  (if (null? exps)
      (make-tagged-exps 'val)
      (let ((exp (car exps)))
        (let ((first-result (one-step-body exp defines-from-context)))
          (let ((first-tag (tag-of first-result)))
            (cond
             ((eq? first-tag 'stepped)
              (let ((body (datum-of-tagged-body first-result)))
                (make-tagged-exps
                 'stepped
                 (defines-of-body body)
                 (cons (expression-of-body body)
                       (cdr exps)))))
             ((eq? first-tag 'val)
              ;;exp must have been an <immediate-value>
              (let ((rest (cdr exps)))
                (let ((rest-result (one-step-exps rest defines-from-context)))
                  (let ((rest-tag (tag-of rest-result)))
                    (cond
                     ((eq? rest-tag 'stepped)
                      (let ((defs-exps (datum-of-tagged-exps rest-result)))
                        (make-tagged-exps
                         'stepped
                         (defines-of-exps-datum defs-exps)
                         (cons
                          exp
                          (expressions-of-exps-datum defs-exps)))))
                     ((eq? rest-tag 'val)
                      ;;all exps must have been <immediate-value>'s
                      (make-tagged-exps 'val))
                     ((eq? rest-tag 'stuck)
                      rest-result))))))
             ((eq? first-tag 'stuck)
              first-result)))))))

  
(define (one-step-combination rator rands defines-from-context)
  (let ((exps-result
         (one-step-exps (append rands (list rator)) defines-from-context)))
    (let ((exps-tag (tag-of exps-result)))
      (cond
       ((eq? exps-tag 'stepped)
        (let ((defs-exps (datum-of-tagged-exps exps-result)))
          (make-tagged-body
           'stepped
           (make-body
            (defines-of-exps-datum defs-exps)
            (let ((exps (expressions-of-exps-datum defs-exps)))
              (make-combination (car (last-pair exps))
                                (except-last-pair exps)))))))
       ((eq? exps-tag 'val)             ;rator and rands are <immediate-value>'s
        (apply-dispatch rator rands))
       ((eq? exps-tag 'stuck)
        exps-result)))))


;; ONE-STEP-DEFINES: (List(<define>),List(<define>)) --> Tagged-Defs
;; Tagged-Defs   =  Labelled-Sum[stepped: List(<define>),
;;                                   val: Empty,
;;                                 stuck: Error-info]

(define (one-step-defines defines defines-from-context)
  (let ((exps-result
         (one-step-exps
          (map expression-of-define defines)
          (append defines-from-context defines))))
    (let ((exps-tag (tag-of exps-result)))
      (cond
       ((eq? exps-tag 'stepped)
        (let ((defs-exps (datum-of-tagged-exps exps-result)))
          (make-tagged-defs
           'stepped
           (append
            (map make-define            ;the stepped defines
                 (map variable-of-define defines)
                 (expressions-of-exps-datum defs-exps))
            (defines-of-exps-datum defs-exps)))))
       ((eq? exps-tag 'val)
        (make-tagged-defs 'val))
       ((eq? exps-tag 'stuck)
        exps-result)))))


;;ONE-STEP-BODY-WITH-DEFINES: (List(<define>),<expression>,List(<define>)) --> Tagged-Body

(define (one-step-body-with-defines defs exp defines-from-context)
  (let ((defs-result
          (one-step-defines defs defines-from-context)))
    (let ((defs-tag (tag-of defs-result)))
      (cond ((eq? defs-tag 'stepped)
             (make-tagged-body
              'stepped
              (make-body
               (defines-of-defs-datum defs-result)
               exp)))
            ((eq? defs-tag 'val)             ;defs must have been <immediate defines>
             (let ((exp-result
                    (one-step-body
                     exp
                     (append defines-from-context defs))))
               (let ((exp-tag (tag-of exp-result)))
                 (cond ((eq? exp-tag 'stepped)
                        (let ((body (datum-of-tagged-body exp-result)))
                          (make-tagged-body
                           'stepped
                           (make-body
                            (append defs (defines-of-body body))
                            (expression-of-body body)))))
                       ((eq? exp-tag 'val)         ;exp must have been a <immediate value>
                        (make-tagged-body
                         'val
                         (garbage-collect defs exp)))
                       ((eq? exp-tag 'stuck)
                        exp-result)))))
            ((eq? defs-tag 'stuck)
             defs-result)))))
     

;;APPLY-DISPATCH: (<immediate value>,List(<immediate value>)) --> Tagged-Body

(define (apply-dispatch rator rands)
  (cond ((lambda-expression? rator)
         (make-tagged-body
          'stepped
          (lambda-apply
           (formals-of-lambda rator)
           (body-of-lambda rator)
           rands)))
        ((rule-specified-procedure-variable? rator)
         (do-rule rator rands))
        ((primitive-procedure-variable? rator)
         (make-tagged-body
          'stepped
          (primitive-apply rator rands)))
        (else (make-tagged-body
               'stuck
               (list 'APPLY-DISPATCH-unknown-rator: rator)))))


;;PRIMITIVE-APPLY:  (<immediate value>,List(<immediate value>)) --> <immediate value>

(define (primitive-apply rator rands)
   (import-from-scheme
    (eval (cons rator rands) user-initial-environment))) ;CALL UNDERLYING SCHEME


;;IMPORT-FROM-SCHEME: Sch-Value --> <immediate-value>

(define (import-from-scheme obj)
  (cond
   ((null? obj)
    submodel-nil)
   ((pair? obj)
    (make-combination
     'cons
     (list (import-from-scheme (car obj))
           (import-from-scheme (cdr obj)))))
   ((symbol? obj)
    (make-symbol obj))
   ((eq? obj (if #f 1))
    submodel-useless-value)             ;#[useless-value]
   (else  obj)))                        ;self-evaluating objects are the same in the submodel
                                        ;and Scheme.  Management is not responsible for
                                        ;imported procedures.


;;LAMBDA-APPLY: (<formals>,<body>, List(<expressions>)) --> <body>
;;creates list of definitions defining successive formals to be
;;successive expressions from the input list.  Adds these definitions
;;to the beginning of body, freshly renaming old and new definitions.

(define (lambda-apply formals body rands)
  (let ((renamed-body (fresh-rename
                       (defines-of-body body)
                       (expression-of-body body))))
    (fresh-rename
     (append (map make-define formals rands)
             (defines-of-body renamed-body))
     (expression-of-body renamed-body))))

;;REWRITE RULES

(define (do-rule rator rands)
  ((cdr (assq rator rule-association-list)) rands))

(define (define-rule-specified-variable! var-name rule-implementing-procedure)
  (set! rule-association-list
        (cons (cons var-name rule-implementing-procedure)
               rule-association-list)))

(define rule-association-list
  (list
   (cons 'cons
         'there-is-no-rule-for-cons)
   (cons 'null?
         (lambda (rands)
           (make-tagged-body
            'stepped
            (submodel-null? (car rands)))))
   (cons 'pair?
         (lambda (rands)
           (make-tagged-body
            'stepped
            ((tagged-pair? 'cons) (car rands)))))
   (cons 'car
         (lambda (rands)
           (let ((rand (car rands)))
             (if ((tagged-pair? 'cons) rand)
                 (make-tagged-body
                  'stepped
                  (car (operands-of rand)))
                 (make-tagged-body
                  'stuck
                  (list 'attempted-car-of: rand))))))
   (cons 'cdr
         (lambda (rands)
           (let ((rand (car rands)))
             (if ((tagged-pair? 'cons) rand)
                 (make-tagged-body
                  'stepped
                  (cadr (operands-of rand)))
                 (make-tagged-body
                  'stuck
                  (list 'attempted-cdr-of: rand))))))
   (cons 'list
         (lambda (rands)
           (make-tagged-body
            'stepped
            (if (null? rands)
                submodel-nil
                (make-combination
                 'cons
                 (list (car rands)
                       (make-combination 'list (cdr rands))))))))
   (cons 'equal?
         (lambda (rands)
           (make-tagged-body
            'stepped
            (equal? (car rands) (cadr rands)))))
   (cons 'apply
         (lambda (rands)
           (define (smlist->list-of-smexps smlist)
	     ;;converts (cons A (cons B ...)) into (A B ...)
             (cond ((submodel-null? smlist)
                    '())
                   ((submodel-null? (caddr smlist))
                    (list (cadr smlist)))
                   (else (cons (cadr smlist)
                               (smlist->list-of-smexps (caddr smlist))))))
           (make-tagged-body
            'stepped
            (make-combination (car rands)
                              (smlist->list-of-smexps (cadr rands))))))
   (cons 'append
         (lambda (rands)
           (cond ((null? rands)
                  (make-tagged-body
                   'stepped
                   submodel-nil))
                 ((null? (cdr rands))
                  (make-tagged-body
                   'stepped
                   (car rands)))
                 (else
                  (let ((rand1 (car rands)))
                    (cond (((tagged-pair? 'cons) rand1)
                           (let ((ivals (operands-of rand1)))
                             (let ((ival1 (car ivals))
                                   (ival2 (cadr ivals)))
                               (make-tagged-body
                                'stepped
                                (make-combination
                                 'cons
                                 (list
                                  ival1
                                  (make-combination
                                   'append
                                   (cons
                                    ival2
                                    (cdr rands)))))))))
                          ((submodel-null? rand1)
                           (make-tagged-body
                            'stepped
                            (make-combination
                             'append
                             (cdr rands))))
                          (else
                           (make-tagged-body
                            'stuck
                            (list 'attempted-append-of: rand1 'to 'arguments (cdr rands))))))))))))


;;GARBAGE-COLLECT: (List(<define>),<expression>) --> <body>

(define (garbage-collect defs exp)
  (let ((initially-needed
         (append
          (free-variables exp)
          (map variable-of-define
               (filter
                (lambda (def)
                  (not
                   (immediate-value?
                    (expression-of-define def))))
                defs)))))
    ;;NEEDED-VARIABLES: Empty --> List(<variables>)
    (define (needed-variables)
      (define (loop explored-needed pending-needed)
        (let ((new-explored (append explored-needed pending-needed))
              (new-needed
               (append-map
                (lambda (var)
                  (let ((var-def (lookup-define var defs)))
                    (if (define? var-def)
                        (free-variables (expression-of-define var-def))
                        '())))
                pending-needed)))
          (let ((new-pending (list-minus new-needed new-explored)))
            (if (null? new-pending)
                new-explored
                (loop new-explored new-pending)))))
      (loop '() initially-needed))
    (let ((needed-vars (needed-variables)))
      (make-body
       (filter
        (lambda (def)
          (member (variable-of-define def) needed-vars))
        defs)
       exp))))

