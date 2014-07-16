;;;ECEVAL.SCM

;;;Ordinary entry to ECEVAL

(define (start-eceval)
  (set! the-global-environment (make-initial-environment))
  (eval-loop))

;;;Return to ECEVAL with the-global-environment intact

(define (eval-loop)
  (fluid-let ((*unparser-list-breadth-limit* 10))
    (set-register-contents! eceval 'flags false)
    (start eceval)))

;;; Arranging for ECEVAL to call compiled code:

(define (compile-and-go expression)
  (let ((instructions
	 (assemble (statements (compile expression 'val 'return))
		   eceval)))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flags true)
    (fluid-let ((*unparser-list-breadth-limit* 10))
      (start eceval))))


(define (get-global-environment)
  the-global-environment)

(define (initialize-eceval-stack)
  (eceval 'initialize-stack)
  (eceval 'initialize-ops-counter))

(define (print-eceval-stack-statistics)
  ((eceval 'stack) 'print-statistics)
  (eceval 'print-ops))

(define eceval-operations
  `((self-evaluating? ,self-evaluating?)
    (quoted? ,quoted?)
    (text-of-quotation ,text-of-quotation)
    (variable? ,variable?)
    (assignment? ,assignment?)
    (assignment-variable ,assignment-variable)
    (assignment-value ,assignment-value)
    (definition? ,definition?)
    (definition-variable ,definition-variable)
    (definition-value ,definition-value)

    (if? ,if?)
    (if-predicate ,if-predicate)
    (if-consequent ,if-consequent)
    (if-alternative ,if-alternative)
    (lambda? ,lambda?)
    (lambda-parameters ,lambda-parameters)
    (lambda-body ,lambda-body)
    (make-procedure ,make-procedure)
    (begin? ,begin?)
    (begin-actions ,begin-actions)

    (first-exp ,first-exp)
    (rest-exps ,rest-exps)
    (last-exp? ,last-exp?)
    (application? ,application?)
    (operator ,operator)
    (operands ,operands)
    (first-operand ,first-operand)
    (rest-operands ,rest-operands)
    (last-operand? ,last-operand?)
    (no-operands? ,no-operands?)
    (no-more-exps? ,no-more-exps?)

    (adjoin-arg ,adjoin-arg)
    (empty-arglist ,empty-arglist)
    (primitive-procedure? ,primitive-procedure?)
    (compound-procedure? ,compound-procedure?)
    (apply-primitive-procedure ,apply-primitive-procedure)
    (procedure-body ,procedure-body)
    (procedure-parameters ,procedure-parameters)
    (procedure-environment ,procedure-environment)
    (extend-environment ,extend-environment)
    (lookup-variable-value ,lookup-variable-value)

    (define-variable! ,define-variable!)
    (set-variable-value! ,set-variable-value!)
    (false? ,false?)
    (list ,list)
    (cons ,cons)

    (display ,display)
    (user-print ,user-print)
    (get-global-environment ,get-global-environment)
    (prompt-for-command-expression ,prompt-for-command-expression)
    (initialize-eceval-stack ,initialize-eceval-stack)
    (print-eceval-stack-statistics ,print-eceval-stack-statistics)
    (compiled-procedure? ,compiled-procedure?)
    (compiled-procedure-entry ,compiled-procedure-entry)
    (compiled-procedure-env ,compiled-procedure-env)
    (make-compiled-procedure ,make-compiled-procedure)

    (cond? ,cond?)
    (cond->if ,cond->if)
    (let? ,let?)
    (let->combination ,let->combination)
   ))


(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
'(
start
  (branch (label external-entry))		;if flags is set

read-eval-print-loop
  (perform (op initialize-eceval-stack))
  (assign exp
	  (op prompt-for-command-expression)
	  (const ";;; EC-Eval input: "))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform (op print-eceval-stack-statistics))
  (perform (op display)
	   (const "\n;;; EC-Eval value: "))
  (perform (op user-print) (reg val))
  (perform (op display) (const "\n"))
  (goto (label read-eval-print-loop))

unknown-expression-type-error
  (restore continue) ; clean up stack (from apply-dispatch)
  (assign val (const 'unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type-error
  (restore continue)		;clean up stack (from apply-dispatch)
  (assign val (const 'unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

external-entry
  (perform (op initialize-eceval-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op let?) (reg exp))
  (branch (label ev-let))
  (test (op application?) (reg exp))
  (branch (label ev-application))

  (goto (label unknown-expression-type-error))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (assign val
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (assign val
   (op define-variable!) (reg unev) (reg val) (reg env))
  (goto (reg continue))

ev-if
  (assign unev (reg exp))
  (save unev)				;save expression for later
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg unev))
  (goto (label eval-dispatch))		;evaluate the predicate
ev-if-decide
  (restore continue)
  (restore env)
  (restore unev)
  (test (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg unev))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg unev))
  (goto (label eval-dispatch))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;; Version that breaks tail recursion
;ev-sequence
;  (test (op no-more-exps?) (reg unev))
;  (branch (label ev-sequence-end))
;  (assign exp (op first-exp) (reg unev))
;  (save unev)
;  (save env)
;  (assign continue (label ev-sequence-continue))
;  (goto (label eval-dispatch))
;ev-sequence-continue
;  (restore env)
;  (restore unev)
;  (assign unev (op rest-exps) (reg unev))
;  (goto (label ev-sequence))
;ev-sequence-end
;  (restore continue)
;  (goto (reg continue))


ev-cond
   (assign exp (op cond->if) (reg exp))
   (goto (label eval-dispatch))


ev-let
   (assign exp (op let->combination) (reg exp))
   (goto (label eval-dispatch))


ev-application
  (save continue)			;dest for after application
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)				;the operands
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))		;go to eval the operator

ev-appl-did-operator
  (restore unev)			;the operands
  (restore env)
  (assign argl (op empty-arglist))	;init argl
  (assign proc (reg val))		;the operator
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))

ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

apply-dispatch  ;;expects continuation to be on the stack
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))  
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type-error))

primitive-apply
  (assign val
          (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

compiled-apply
   (restore continue)   ;compiled code body assumes continuation in continue
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))

)))


