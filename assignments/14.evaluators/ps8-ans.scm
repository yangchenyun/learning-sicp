;a possible test for AND-PROC correctness:

;(and-proc (begin (pp 1) #t) (begin (pp 2) #f) (begin (pp 3)))


;;; For defining, using dotted-list formal params, procedures
;;; with varying numnber of args 

(define (process-arg-procs params aprocs env)
  (let ((params (undot params))
        (aprocs (matchup-args params aprocs)))
                          ;now back to the old process-arg-procs body:
    (map (lambda (param aproc)
           (cond ((variable? param) (aproc env))
                 ((lazy? param) (delay-it aproc env))
                 ((memo? param) (delay-it-memo aproc env))
                 (else (error "Unknown declaration" param))))
         params
         aprocs)))


       ;convert a possibly dotted list of aproc's into a list of aproc's
       ;slightly revised from PS8 for clarity
(define (matchup-args params aprocs)
  (cond
   ((null? params)
    (if (null? aprocs)
        '()
        (error "matchup-args: too many args" aprocs)))
   ((or (variable? params) (declaration? params))
    (list (make-list-package params aprocs)))
   ((null? aprocs)
    (error "matchup-args: too few args" params))
   (else (cons
          (car aprocs)
          (matchup-args (cdr params) (cdr aprocs))))))


(define (make-list-package formal aprocs)
 (lambda (env)
       (map
        (cond ((variable? formal)
               (lambda (aproc) (aproc env)))
              ((lazy? formal)
               (lambda (aproc) (delay-it aproc env)))
              ((memo? formal)
               (lambda (aproc) (delay-it-memo aproc env))))
        aprocs)))


;(define (undot params)
;  <blob6a>)


;; ;a possible test for AND correctness:

;(and (begin (pp 1) #t) (begin (pp 2) #t) (begin (pp 3) #f) (begin (pp 4)))
