;;;;SMSCOPE.SCM   Code for Sample Pset
;;; Procedures for Variable Binding with Lexical Scope in Substitution Model 

                                        ;FOR THE COMPILER

(declare (usual-integrations))
(declare (integrate-external "smfresh"))
(declare (integrate-external "smsyntax"))
(declare (integrate-operator list-minus))
(declare (integrate-operator filter))

;;UTILITIES

;;LIST-MINUS: (List(S),List(T)) --> List(S)
(define (list-minus l1 l2) ; returns members of l1 not in l2
  (filter
   (lambda (v) (not (member v l2)))
   l1))

;;FILTER: ((S --> Sch-Bool), List(S)) --> List(S)
;;(filter pred lst) returns list of element in LST satisfying PRED

(define filter (lambda (pred list) (list-transform-positive list pred)))


;;FREE-VARIABLES: <body> --> List(<variable>)
;;Works only on kernel (desugared) bodies.

(define (free-variables body)
  (cond ((or (self-evaluating? body)
             (symbol-expression? body)
             (submodel-null? body))
         '())
        ((variable? body)
         (list body))
        ((combination? body)
         (append-map free-variables
                     (cons (operator-of body) (operands-of body))))
        ((if? body)
         (append-map free-variables
                     (list (test-of-if body)
                           (consequent-of body)
                           (alternative-of body))))
        ((lambda-expression? body)
         (list-minus (free-variables (body-of-lambda body))
                     (formals-of-lambda body)))
        (else                           ;body with defs
         (list-minus
          (append
           (append-map
            (lambda (def)
              (free-variables (expression-of-define def)))
            (defines-of-body body))
           (free-variables (expression-of-body body)))
          (map variable-of-define (defines-of-body body))))))


;;FRESH-RENAME: (List(<define>),<body>) --> <body>
;;assigns fresh names in the list of defines and in the body
;;to variables defined in the list of define's.  Returns renamed defines
;;appended to body

(define (fresh-rename defs body)
  (if (null? defs)
      body
      (let ((vrbls (map variable-of-define defs))
            (exps (map expression-of-define defs)))
        (let ((fresh-vars (map get-fresh vrbls)))
          (let ((renaming-operator
                 (make-renaming-operator
                  (map list
                       vrbls
                       fresh-vars))))
            (let ((renamed-body (renaming-operator body)))
              (make-body
               (append
                (map make-define
                     fresh-vars
                     (map renaming-operator exps))
                (defines-of-body renamed-body))
               (expression-of-body renamed-body))))))))



;;GET-FRESH: <symbol> --> <symbol>
;;  The procedure GET-FRESH is provided as a primitive procedure.  A call
;;                    (get-fresh 'name)
;;  returns a symbol of the form "name#<num>", which differs
;;  from any symbol previously returned by a call to GET-FRESH.

;;  For example,
;;  (get-fresh 'z) ==>z#0
;;  (get-fresh 'z) ==>z#1
;;  (get-fresh 'hello) ==>hello#0


;;Renaming = List(<variable> x <variable>)
;;Renaming-Operator: <body> --> <body>

;;MAKE-RENAMING-OPERATOR: Renaming --> Renaming-Operator
;;The renaming-operator returned will work only on kernel (desugared) bodies.

(define (make-renaming-operator renaming)
  (lambda (body)
    (cond ((or (self-evaluating? body)
               (symbol-expression? body)
               (submodel-null? body))
           body)
          ((variable? body)
           (let ((binding (assq body renaming)))
             (if (pair? binding)
                 (cadr binding)
                 body)))
          ((combination? body)
           (make-combination
            ((make-renaming-operator renaming) (operator-of body))
            (map (make-renaming-operator renaming) (operands-of body))))
          ((lambda-expression? body)
           (let ((formals (formals-of-lambda body)))
             (make-lambda
              formals
              ((let ((bindings-minus-formals
                      (filter
                       (lambda (binding)
                         (not (memq (car binding) formals)))
                       renaming)))
                 (make-renaming-operator
                  bindings-minus-formals))
               (body-of-lambda body)))))
          ((if? body)
           (make-if
            ((make-renaming-operator renaming) (test-of-if body))
            ((make-renaming-operator renaming) (consequent-of body))
            ((make-renaming-operator renaming) (alternative-of body))))
          (else
           (let ((defs (defines-of-body body)))
             (let ((vars (map variable-of-define defs)))
               (let ((renaming-operator
                      (make-renaming-operator
                       (filter
                        (lambda (binding)
                          (not (memq (car binding) vars)))
                        renaming))))
                 (make-body
                  (map make-define
                       vars
                       (map renaming-operator (map expression-of-define defs)))
                  (renaming-operator (expression-of-body body))))))))))


;;ENFORCE: <body> --> <body>
;;Enforces the VARIABLE CONVENTION on a Scheme body:
;;           no variable is bound more than once, and
;;           no bound variable is the same as any free variable.
;;Works only on kernel (desugared) bodies.

(define (enforce body)
  (cond ((or (self-evaluating? body)
             (symbol-expression? body)
             (variable? body)
             (submodel-null? body))
         body)
        ((combination? body)
         (make-combination
          (enforce (operator-of body))
          (map enforce (operands-of body))))
        ((lambda-expression? body)
         (let ((done-lam-body (enforce (body-of-lambda body)))
               (formals (formals-of-lambda body)))
           (let ((fresh-formals (map get-fresh formals)))
             (make-lambda
              fresh-formals
              (let ((fresh-renaming (map list formals fresh-formals)))
                ((make-renaming-operator fresh-renaming) done-lam-body))))))
        ((if? body)
         (make-if
          (enforce (test-of-if body))
          (enforce (consequent-of body))
          (enforce (alternative-of body))))
        (else
         (let ((defs (defines-of-body body))
               (done-exp (enforce (expression-of-body body))))
           (let ((done-exps (map enforce (map expression-of-define defs)))
                 (vars (map variable-of-define defs)))
             (let ((fresh-vars (map get-fresh vars)))
               (let ((fresh-renaming (map list vars fresh-vars)))
                 (let ((renaming-operator (make-renaming-operator fresh-renaming)))
                   (make-body
                    (map make-define
                         fresh-vars
                         (map renaming-operator done-exps))
                    (renaming-operator done-exp))))))))))


;;  GET-CLEANED-VARIABLE: (<variable>, List(<variable>)) --> <variable>
;;  The procedure GET-CLEANED-VARIABLE is provided as a primitive.
;;  (get-cleaned-variable 'prefix#<num> vars-to-avoid) returns a symbol
;;  of the form "prefix", or "prefix#m" with minimal m, which is not in vars-to-avoid.
;;  For example,
;;  (get-cleaned-variable 'x#2 '()) ==> x
;;  (get-cleaned-variable 'x#2 '(x x#3 y)) ==> x#0
;;  (get-cleaned-variable 'x#2 '(x#0 x#3 x#1 y x)) ==> x#2
;;  (get-cleaned-variable 'y '(x#0 x#3 x#1 y x)) ==> y#0


;;  MAP-GET-CLEANED: (List(<variable>), List(<variable>)) --> <variable>
;;  (get-cleaned-variable vars vars-to-avoid) returns a list of symbols
;;  of the same length as vars and with the same prefix as the corresponding
;;  symbol in vars.  The symbols in the returned list have the minimum suffixes
;;  such that they are distinct from each other and the symbols in vars-to-avoid.

;;  For example,
;;  (map-get-cleaned '(x#2 y y#2) '()) ==> (x y y#0)
;;  (map-get-cleaned '(x#2 y y#2) '(x#0 x#3 x#1 y x)) ==> (x#2 y#0 y#1)


(define (map-get-cleaned vars vars-to-avoid)
  (if (null? vars)
      '()
      (let ((first-cleaned-var
             (get-cleaned-variable (car vars) vars-to-avoid)))
        (cons first-cleaned-var
              (map-get-cleaned (cdr vars) (cons first-cleaned-var vars-to-avoid))))))


;;;;  CLEAN-SUFFIXES.SCM

;; Return the body with its free variables unchanged, but its bound variables
;; renamed with no, or minimum, suffixes.  It will undo ENFORCE.
;; Works only on kernel (desugared) bodies.

(define (clean-suffixes body)
  (cond ((or (self-evaluating? body)
             (symbol-expression? body)
             (variable? body)
             (submodel-null? body))
         body)
        ((combination? body)
         (make-combination
          (clean-suffixes (operator-of body))
          (map clean-suffixes (operands-of body))))
        ((lambda-expression? body)
         (let ((cleaned-lam-body (clean-suffixes (body-of-lambda body)))
               (formals (formals-of-lambda body)))
           (let ((vars-to-avoid
                  (list-minus (free-variables cleaned-lam-body) formals)))
             (let ((cleaned-formals (map-get-cleaned formals vars-to-avoid)))
               (make-lambda
                cleaned-formals
                (let ((cleaned-renaming (map list formals cleaned-formals)))
                  ((make-renaming-operator cleaned-renaming) cleaned-lam-body)))))))
        ((if? body)
         (make-if
          (clean-suffixes (test-of-if body))
          (clean-suffixes (consequent-of body))
          (clean-suffixes (alternative-of body))))
        (else
         (let ((defs (defines-of-body body))
               (cleaned-exp (clean-suffixes (expression-of-body body))))
           (let ((cleaned-exps (map clean-suffixes (map expression-of-define defs)))
                 (vars (map variable-of-define defs)))
             (let ((cleaned-vars
                    (map-get-cleaned vars
                                     (list-minus
                                      (append
                                       (free-variables cleaned-exp)
                                       (append-map free-variables cleaned-exps))
                                      vars))))
               (let ((cleaned-renaming (map list vars cleaned-vars)))
                 (let ((renaming-operator (make-renaming-operator cleaned-renaming)))
                   (make-body
                    (map make-define
                         cleaned-vars
                         (map renaming-operator cleaned-exps))
                    (renaming-operator cleaned-exp))))))))))


