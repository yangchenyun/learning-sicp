;REGSIM.SCM

;;;;  6.001 Register Machine Simulator
;;;;       With Tracing Added


(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flags (make-register 'flags))
        (stack (make-stack))
        (the-ops '())
        (the-instruction-sequence '())
        (machine-ops 0)
        (trace-flag false)
	(traced-regs '()))
    (let ((register-table
	   (list (list 'pc pc) (list 'flags flags))))
      (define (allocate-register name)
        (if (assq name register-table)
            (error "Multiply-defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (execute)
        (let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin (set! machine-ops (inc machine-ops))
		     (if trace-flag    ; Print code
			 (write-line 
			  (list 'executing: (instruction-text (car insts))))
			 'not-traced)
		     ((instruction-execution-proc (car insts)))
		     (let ((inst (instruction-text (car insts))))
		       (if (and (eq? 'assign (car inst))
				(memq (cadr inst) traced-regs))
			   (write-line (list 'assigned:
					     (list (cadr inst)
						   '=
						   (get-register-contents
						    dispatch
						    (cadr inst)))))
			   'not-traced))
		     (execute)
		     ))))
      (define (dispatch message)
        (cond ((eq? message 'initialize-stack) (stack 'initialize))
	      ((eq? message 'print-stack-statistics)
	       (stack 'print-statistics))
              ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'register-table) register-table)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops ops)))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'instruction-sequence)
               the-instruction-sequence)

              ;; Debugging aids
              ((eq? message 'initialize-ops-counter) 
               (set! machine-ops 0)
               'ops-counter-initialized)
	      ((eq? message 'print-ops)
               (write-line (list 'machine-ops '= machine-ops))
	       'ops-printed)
              ((eq? message 'trace-on)
               (set! trace-flag true)
               'tracing)
              ((eq? message 'trace-off)
               (set! trace-flag false)
               'not-tracing)

	      ((eq? message 'trace-reg-on)
	       (lambda (reg)
		 (set! traced-regs (cons reg traced-regs))))

	      ((eq? message 'trace-reg-off)
	       (lambda (reg)
		 (set! traced-regs (delq reg traced-regs))))

              (else (error "unknown operation -- MACHINE"))))
      dispatch)))


(define (trace-on machine) (machine 'trace-on))
(define (trace-off machine) (machine 'trace-off))
(define (trace-reg-on machine reg) ((machine 'trace-reg-on) reg))
(define (trace-reg-off machine reg) ((machine 'trace-reg-off) reg))

;;; Rest of simulator interface

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine register-name)
  (let ((val (assq register-name (machine 'register-table))))
    (if val
	(cadr val)
	(error "Unknown register" register-name))))


;;; some primitives for use in register machines

(define standard-primitives
  `((+ ,+)
    (- ,-)
    (* ,*)
    (/ ,/)
    (inc ,inc)
    (dec ,dec)
    (= ,=)
    (< ,<)
    (> ,>)
    (zero? ,zero?)
    (cons ,cons)
    (car ,car)
    (cdr ,cdr)
    (pair? ,pair?)
    (null? ,null?)
    (list ,list)
    (eq? ,eq?)
    (symbol? ,symbol?)
    (write-line ,write-line)
    (not ,not)
    (true ,true)
    (false ,false)
    ))


;;; Stack Model -- monitored stack

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (write-line (list 'total-pushes  '= number-pushes
                        'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
	    ((eq? message 'print-statistics) (print-statistics))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


;;;REGISTERS

(define (make-register name)
  (let ((contents '()))
    (define (get) contents)
    (define (set value)
      (set! contents value)
      value)
    (define (dispatch message)
      (cond ((eq? message 'get) (get))    
            ((eq? message 'set) set)
            (else
             (error "Unknown request -- REGISTER" name message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))
