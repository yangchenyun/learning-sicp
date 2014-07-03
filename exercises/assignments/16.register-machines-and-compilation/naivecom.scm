;;;;NAIVECOM.SCM

                        ;;;A Naive Code Generator

; By dropping most of the optimizations from the compiler in the text, we
; get a simpler compilation procedure, NAIVE-COMPILE, which helps clarify
; the core ideas of how Scheme programs are translated into register machine
; code.
; 
; *** The compilation procedures below take an expression as their only
; parameter;  each expression compiles in only one way.
; 
; *** Compiled expressions and compiled procedures each fulfill the
; SAME contract wrt register and stack use.
; 
; *** Compiled code is tail recursive.
; 
; *** It is fairly easy to see what the generated register-machine code will
; look like, because the bodies of the subprocedures of NAIVE-COMPILE look
; very much like the code they generate;  this is accomplished by making
; elementary use of BACKQUOTE.
; 
; *** NAIVE-COMPILE is independent of, and not compatible with, the
; ECEVAL interpreter of the Notes because the register-code it generates
; obeys a different contract than expected by ECEVAL.  Compatibility could
; be achieved with a local change in the ECEVAL apply-dispatch code to
; handle NAIVE-COMPILE procedures as a separate case.
; 
; 
;                               THE CONTRACT
; 
; (NAIVE-COMPILE EXP) yields register machine code such that
; 
; *** if register ENV contains an environment, and
; 
; *** register CONTINUE contains a label,
; 
; *** then execution of the code evaluates EXP, causing appropriate
; mutations of the environment and store, if any, and leaves the value in
; VAL.
; 
; ***  Execution preserves the stack.
; 
; ***  At the end of execution, control transfers to the label originally in
; CONTINUE.
; 
; *** All labels explicitly in the code are ``fresh'' (do not appear in
; other code fragments).


(define (naive-compile exp)
  (cond ((self-evaluating? exp)
         (naive-compile-self-evaluating exp))
        ((quoted? exp)
         (naive-compile-quoted (text-of-quotation exp)))
        ((variable? exp)
         (naive-compile-variable-access exp))
        ((assignment? exp)
         (naive-compile-assignment
          (assignment-variable exp)
          (assignment-value exp)))
        ((definition? exp)
         (naive-compile-definition
          (definition-variable exp)
          (definition-value exp)))
        ((if? exp)
         (naive-compile-if
          (if-predicate exp)
          (if-consequent exp)
          (if-alternative exp)))
        ((begin? exp)
         (if (no-more-exps? (begin-actions exp))
             (error "Empty sequence of actions -- NAIVE-COMPILE" exp)
             (naive-compile-seq (begin-actions exp))))
        ((cond? exp)
         (naive-compile (cond->if exp)))
        ((lambda? exp)
         (let ((params (lambda-parameters exp))
               (body (lambda-body exp)))
           (if (no-more-exps? body)
               (error "Empty lambda body -- NAIVE-COMPILE" exp))
           (naive-compile-lambda params body)))
        ((let? exp)
         (naive-compile (let->combination exp)))
        ((application? exp)
         (naive-compile-application
          (operator exp)
          (operands exp)))
        (else
         (error "Unknown expression type -- NAIVE-COMPILE" exp))))

(define (naive-compile-self-evaluating constant)
  `((assign val (const ,constant))
   (goto (reg continue))))

(define (naive-compile-quoted text)
  `((assign val (const ,text))
    (goto (reg continue))))

(define (naive-compile-variable-access var)
  `((assign val (op lookup-variable-value) (const ,var) (reg env))
    (goto (reg continue))))

(define (naive-compile-assignment assignment-var assignment-valexp)
  (let ((after-valexp-label (make-label 'after-valexp)))
    (append
     `((save continue)
       (assign continue (label ,after-valexp-label))
       (save env))
     (naive-compile assignment-valexp)
     `(,after-valexp-label
       (restore env)
       (perform (op set-variable-value!) (const ,assignment-var) (reg val) (reg env))
       (assign val (const ,the-unspecified-value))
       (restore continue)
       (goto (reg continue))))))

(define (naive-compile-definition def-var def-valexp)
  (let ((after-def-value-label (make-label 'after-def-value)))
    (append
     `((save continue)
       (assign continue (label ,after-def-value-label))
       (save env))
     (naive-compile def-valexp)
     `(,after-def-value-label
       (restore env)
       (perform (op define-variable!) (const ,def-var) (reg val) (reg env))
       (assign val (const ,the-unspecified-value))
       (restore continue)
       (goto (reg continue))))))
   
(define (naive-compile-if pred true-brnch false-brnch)
  (let ((after-pred-label (make-label 'after-pred))
        (false-branch-label (make-label 'false-branch)))
    (append
     `((save continue)
       (assign continue (label ,after-pred-label))
       (save env))
     (naive-compile pred)
     `(,after-pred-label
       (restore env)
       (restore continue)
       (test (op false?) (reg val))
       (branch (label ,false-branch-label)))
     (naive-compile true-brnch)
     `(,false-branch-label)
     (naive-compile false-brnch))))

(define (naive-compile-seq explist)
  (append
   '((save continue))
   (append-map
    (lambda (exp)
      (let ((after-current-exp-label (make-label 'after-current-exp)))
        (append
         `((save env)
           (assign continue (label ,after-current-exp-label)))
         (naive-compile exp)
         `(,after-current-exp-label
           (restore env)))))
    (all-but-last explist))
   '((restore continue))
   (naive-compile (last-exp explist))))

(define (all-but-last explist)
  (if (last-exp? explist)
      '()
      (cons (first-exp explist)
            (all-but-last (rest-exps explist)))))

(define (last-exp explist)
  (list-ref explist (dec (length explist))))

(define (naive-compile-lambda params body)
  (let ((proc-entry-label (make-label 'proc-entry)))
    (append
     `((assign val (op make-compiled-procedure) (label ,proc-entry-label) (reg env))
       (goto (reg continue))
       ,proc-entry-label
               ; This code expects the procedure-object environment in ENV, a
               ; list of arguments in ARGL, and its continuation label in
               ; CONTINUE.  It evaluates the body of abstraction in the
               ; procedure object's extended environment, leaving its value
               ; in VAL.  It preserves the stack.  Finally it goes to
               ; CONTINUE.
       (assign env
               (op extend-environment)
               (const ,params)
               (reg argl)
               (reg env)))
     (naive-compile-seq body))))

(define (naive-compile-application rator rands)
  (let ((after-rator-label (make-label 'after-rator))
        (primitive-apply-label (make-label 'primitive-apply)))
    (append
     `((save continue)
       (assign argl (op empty-arglist)))
     (naive-compile-operands rands)
     `((assign continue (label ,after-rator-label))
       (save argl))
     (naive-compile rator)
     `(,after-rator-label
       (restore argl)
       (restore continue)
       (test (op primitive-procedure?) (reg val))
       (branch (label ,primitive-apply-label))
                                         ; doesn't handle interpreted procedures.
       (assign env (op compiled-procedure-env) (reg val))
       (assign val (op compiled-procedure-entry) (reg val))
       (goto (reg val))
       ,primitive-apply-label
       (assign val (op apply-primitive-procedure) (reg val) (reg argl))
       (goto (reg continue))))))


               ; This code evaluates rands, adjoining their values to ARGL.
               ; It preserves STACK and ENV, returns to next instruction.
(define (naive-compile-operands rands)
  (append-map
   (lambda (rand)
     (let ((after-rand-label (make-label 'after-rand)))
       (append
        `((assign continue (label ,after-rand-label))
          (save env)
          (save argl))
        (naive-compile rand)
        `(,after-rand-label
          (restore argl)
          (assign argl (op adjoin-arg) (reg val) (reg argl))
          (restore env)))))
   rands))


                  ; PROCEDURES FOR RUNNING THE COMPILER

(define (naive-compile-and-display expression)
  (fluid-let ((*unparser-list-breadth-limit* #f)
	      (*unparse-uninterned-symbols-by-name?* #T))
    (newline)
    (for-each
	(lambda (statement)
	  (newline)
	  (cond ((pair? statement)
		 (display "  ")
		 (display statement))
		(else (display statement))))
      (naive-compile expression))))


(define compiler-ops
  `((lookup-variable-value ,lookup-variable-value)
    (set-variable-value! ,set-variable-value!)
    (define-variable! ,define-variable!)
    (false? ,false?)
    (empty-arglist ,empty-arglist)
    (adjoin-arg ,adjoin-arg)
    (make-compiled-procedure ,make-compiled-procedure)
    (extend-environment ,extend-environment)
    (compiled-procedure-entry ,compiled-procedure-entry)
    (compiled-procedure-env ,compiled-procedure-env)
    (primitive-procedure? ,primitive-procedure?)
    (apply-primitive-procedure ,apply-primitive-procedure)))


(define (naive-compile-and-go exp)
  (let ((naive-machine
         (make-machine '(val env continue argl)
                       compiler-ops
                       (naive-compile exp))))
    (set-register-contents! naive-machine 'env (make-initial-environment))
    (set-register-contents! naive-machine 'continue '())
    (naive-machine 'initialize-stack)
    (naive-machine 'initialize-ops-counter)
    (start naive-machine)
    (naive-machine 'print-stack-statistics)
    (naive-machine 'print-ops)
    (newline)
    (display ";value of expression: ")
    (user-print (get-register-contents naive-machine 'val))))
