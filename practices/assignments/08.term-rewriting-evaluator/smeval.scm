;;;;SMEVAL.SCM   Code for Sample Pset
;;;  EVAL-PRINT loop for Substitution Model 

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



;;SMSTEP-LIST repeatedly calls ONE-STEP, cons'ing up selected Step's
;;until reaching a value or getting stuck or interrupted.

;;SMSTEP-LIST: <body> --> Tagged-Steps

;; Tagged-Steps  =  Step-Tag  x  List(Step)
;; Step-Tag = {interrupted, val, stuck}
;;constructor: MAKE-TAGGED-STEPS: (Step-Tag, List(Step)) --> Tagged-Steps
;;selector: DATUM-OF-TAGGED-STEPS: Tagged-Steps --> List(Step)

;; Step = Sch-Positive-Integer x (<body> U Error-info)
;; constructor MAKE-STEP: (Sch-Positive-Integer, (<body> U Error-Info)) --> Step
;; selectors   STEP-NUMBER-OF: Step --> Sch-Positive-Integer
;;             BODY-OR-INFO-OF-STEP: Step --> (<body> U Error-Info)


(define (smstep-list body)
  (define (do-steps step-number body steps-list)
    (let ((tagged-body (one-step-body body global-definitions)))
      (let ((tag (tag-of tagged-body))
            (next-body (maybe-collect-garbage
                        step-number
                        (datum-of-tagged-body tagged-body))))
	(let ((new-steps-list (add-step
                               tag
                               step-number
                               next-body
                               steps-list)))
          (cond ((memq tag '(val stuck))
                 (make-tagged-steps tag new-steps-list))
                ((interrupt-smeval? step-number next-body)
                 (make-tagged-steps 'interrupted new-steps-list))
                (else (do-steps (inc step-number) next-body new-steps-list)))))))
  (let ((desugared-body (desugar body)))
    (do-steps
     1
     desugared-body
     (list (make-step 0 desugared-body)))))

(define (maybe-collect-garbage number body)
  (if (garbage-collect-this-step? number body)
      (garbage-collect
       (defines-of-body body)
       (expression-of-body body))
      body))

(define (add-step tag number body steps)
  (if (or (memq tag '(val stuck))
	  (save-this-step? number body))
      (cons (make-step number body) steps)
      steps))

;;;THE GLOBAL DEFINITIONS: List(<immediate defines>)
;;;These are all user-definable, but are supplied for convenience
;;;and to minimize clutter in the printout.  By being installed here,
;;;they aren't printed.

;;;They must be desugared before being included in the list.

(define global-definitions
  '((define (list? obj)
      (if (null? obj)
          #t
          (if (pair? obj)
              (list? (cdr obj))
              #f)))
    (define (map f l)                   ;works only for f taking 1 argument
      (if (null? l)
          ()
          (cons (f (car l))
                (map f (cdr l)))))))


     ;;SAVED STEP PREDICATE

(define (save-this-step? n body)        ;cons at the rate
  (or (zero? n)
      (let ((sqn (round (sqrt n))))	;1/ sqrt(n)
	(zero? (modulo n sqn)))))

    ;;SUBMODEL INTERRUPT PREDICATE

(define (interrupt-smeval? step-number body)
  (> step-number 600))

    ;;GARBAGE COLLECTION IMPOSED-RATE PREDICATE

(define (garbage-collect-this-step? step-number body)   ;impose a garbage collection
  (zero? (modulo step-number 40)))                      ;every 40th step



                               ;;;;CONTROLLING THE PRINTOUT

;;PRINT-STEPS: Tagged-Steps --> <body>

;;SMEVAL: <body> --> <body>
;;nicely prints out (SMSTEP-LIST BODY), returning the final result of the rewriting

(define (smeval body) (print-steps (smstep-list body)))

(define (print-steps tagged-steplist)
    (let ((tag (tag-of tagged-steplist))
          (reversed-steps (datum-of-tagged-steps tagged-steplist)))
      (let ((final-body (body-or-info-of-step (car reversed-steps)))
            (steps (reverse reversed-steps)))
        (define (printlst stlst)
          (if (null? stlst)
              (print-final-message tag)
              (let ((current-step (car stlst)))
                (print-stepped-message
                 (step-number-of current-step)
                 (body-or-info-of-step current-step))
                (printlst (cdr stlst)))))
        (printlst steps)
        final-body)))


(define (print-stepped-message step-number body)
  (begin (newline)
         (newline)
         (display ";==(")
         (display step-number)
         (display ")==>")
         (pp body)))


(define (print-final-message tag)
  (newline)
  (display
   (cond
    ((eq? tag 'val) "Syntactic Value was returned")
    ((eq? tag 'interrupted) "Rewriting got interrupted")
    ((eq? tag 'stuck) "Rewriting got stuck"))))


    ;;PRINTABLE-VERSION: <value> --> Nested-List(Sch-Symbol + Sch-Num + Sch-Bool + '() )
     ;;Make a Sub Model value look more like what Scheme would print out

(define (printable-version value)
  (cond (((tagged-pair? 'cons) value)
         (if (submodel-null? (caddr value))
             (list (printable-version (cadr value)))
             (cons
              (printable-version (cadr value))
              (printable-version (caddr value)))))
        ((symbol-expression? value)
         (cadr value))
        ((submodel-null? value)
         '())
        ((or (lambda-expression? value)
             (primitive-procedure-variable? value)
             (rule-specified-procedure-variable? value))
         (cons 'procedure-object: value))
        ((and (not (expression? value))
              (pair? value)
              (pair? (car value))
              (define? (caar value))) ;VALUE is a body with defines
         (printable-version (expression-of-body value))) ;;don't print the defines
        (else value)))                  ;not sure what it is, so leave it alone
















