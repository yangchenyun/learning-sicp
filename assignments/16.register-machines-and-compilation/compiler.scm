;;;;COMPILER.SCM
;;; compiled code is not able to call interpreted procedures.
(declare (usual-integrations))

;;; this compiles and then displays the code
(define (compile-and-display expression)
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
      (statements (compile expression 'val 'return)))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage))
        ((assignment? exp) (compile-assignment exp target linkage))
        ((definition? exp) (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp) (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile (COND->IF exp) target linkage))
        ((let? exp) (compile (LET->combination exp) target linkage))
        ((application? exp) (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-self-evaluating exp target linkage)
  (preserving '(continue)
              (make-instruction-sequence '() (list target)
               `((assign ,target (const ,exp))))
              (compile-linkage linkage)))

(define (compile-quoted exp target linkage)
  (preserving '(continue)
              (make-instruction-sequence '() (list target)
               `((assign ,target (const ,(text-of-quotation exp)))))
              (compile-linkage linkage)))

(define (compile-variable exp target linkage)
  (preserving '(continue)
              (make-instruction-sequence '(env) (list target)
               `((assign
                  ,target
                  (op lookup-variable-value) (const ,exp) (reg env))))
              (compile-linkage linkage)))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         `((assign argl (op empty-arglist))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl)
           '(argl)
           `((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))


(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
    (append-instruction-sequences
     (make-instruction-sequence '(proc) '()
      `((test (op primitive-procedure?) (reg proc))
        (branch (label ,primitive-branch))))
     (parallel-instruction-sequences
      (append-instruction-sequences
       compiled-branch
       (compile-proc-appl target compiled-linkage))
      (append-instruction-sequences
       primitive-branch
       (make-instruction-sequence '(proc argl) (list target)
        `((assign ,target
                  (op apply-primitive-procedure)
                  (reg proc)
                  (reg argl))))
       (compile-linkage linkage)))
     after-call))))


(define (compile-proc-appl target linkage)
  (if (eq? linkage 'next)
      (error "Unexpected linkage = next -- COMPILE-PROC-APPL"))
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry) (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE-PROC-APPL"
                target))

         #|
         ;; Convention of returned values in VAL prevents this case from happening. 
         (let ((proc-return (make-label 'proc-return)))
           (preserving '(continue)
            (make-instruction-sequence '(proc) all-regs
             `((assign continue (label ,proc-return))
               (assign val (op compiled-procedure-entry) (reg proc))
               (goto (reg val))
               ,proc-return
               (assign ,target (reg val))))
            (make-instruction-sequence '(continue) all-regs
             `((goto (reg continue))))))
         |#
	 ))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next)))
    (preserving '(env continue)
     get-value-code
     (preserving '(continue)
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target  (const ,the-unspecified-value))))
      (compile-linkage linkage)))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next)))
    (preserving '(env continue)
     get-value-code
     (preserving '(continue)
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ,the-unspecified-value))))
      (compile-linkage linkage)))))


(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (if (eq? linkage 'next)
      (let ((after-lambda (make-label 'after-lambda)))
        (append-instruction-sequences
         (compile-lambda-2 exp  target after-lambda)
         after-lambda))
      (compile-lambda-2 exp target linkage)))

(define (compile-lambda-2 exp  target linkage)
  (let ((proc-entry (make-label 'entry)))
    (tack-on-instruction-sequence
     (append-instruction-sequences
      (make-instruction-sequence '(env) (list target)
       `((assign ,target
                 (op make-compiled-procedure)
                 (label ,proc-entry)
                 (reg env))))
      (compile-linkage linkage))
     (compile-lambda-body exp  proc-entry))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (preserving '(continue)
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env
		(op extend-environment)
		(const ,formals)
		(reg argl)
		(reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

