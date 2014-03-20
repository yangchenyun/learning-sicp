;;; TRY-RULES tries each of a list of rules on a given datum. 
;;; If any rule is applicable, the success continuation, SUCCEED, is
;;; called with a value and a failure continuation.  If no rule is 
;;; applicable, FAIL is called with no arguments.

;;; A rule is an association of a pattern with a procedure to
;;; be executed if the expression matches the pattern.  The rule
;;; procedure gets a dictionary as its argument.  The dictionary
;;; represents the map of pattern variables to the matched values.
;;; The rule procedure may itself either succeed or fail. 

(define (try-rules datum the-rules fail succeed)
  ;; fail = (lambda () ...)
  ;; succeed = (lambda (value fail) ...)
  (define (scan rules)
    (if (null? rules)
	(fail)
	(match (rule-pattern (car rules)) datum (make-empty-dictionary)
	       (lambda () (scan (cdr rules)))
	       (lambda (dictionary fail)
		 (succeed ((rule-procedure (car rules)) dictionary)
			  fail)))))
  (scan the-rules))

;;; The matcher takes a pattern, data, dictionary, 
;;;  and success and failure continutation procedures.
;;;    DICTIONARY is a list of triples: (var value info)
;;;    FAIL is (lambda () ...)
;;;    SUCCEED is (lambda (dict fail) ....)
;;; The idea is that the succeed continuation passes along the place
;;;  to fail back to, if we need to backtrack later.

(define (match pat dat dict fail succeed)
  (cond ((eq? pat dat)
	 (succeed dict fail))
        ((arbitrary-element? pat)
         (element-match pat dat dict fail succeed))
        ((constant? pat)
         (if (same-constant? pat dat)
	     (succeed dict fail)
	     (fail)))
        ((start-arbitrary-segment? pat)
	 (segment-match (car pat) dat dict 
			fail 
			(lambda (rest dict fail)
			  (match (cdr pat) rest dict
				 fail succeed))))
        ((pair? dat)
	 (match (car pat) (car dat) dict
		fail
		(lambda (dict fail)
		  (match (cdr pat) (cdr dat) dict
			 fail succeed))))
        (else
	 (fail))))


;;; An arbitrary element pattern may have a restriction -- a procedure
;;;  that determines whether or not the datum should be allowed
;;;  to match the pattern.  The restriction may either succeed or fail.
;;;  If it succeeds, the success continuation is called with a value, 
;;;  which is added to the dictionary in the info field.

(define (element-match var dat dict fail succeed)
  (let ((vname (var-name var)))
    (let ((e (lookup vname dict)))
      (if e				; If already bound
	  (let ((val (matcher-entry-data e)))	; current value must
	    (if (equal? val dat)	; be same as expression.
		(succeed dict fail)
		(fail)))
	  ((var-restriction var)
	   dat
	   fail
	   (lambda (result fail)
	     (succeed (extend-dictionary vname dat result dict) fail)))))))

;;; Try to match some initial segment of the data.  
;;; The success continuation for this is of the form
;;;   (lambda (rest-of-data dictionary fail) ...)
;;; Thus, if the match succeeds, we go on to try to match the rest of
;;; the data against the rest of the pattern, while keeping the
;;; failure point as a place to backtrack to.

(define (segment-match seg-var dat dict fail succeed-with-rest)
  (let ((vname (var-name seg-var))
	(p (var-restriction seg-var)))
    (define (try-segment rest)
      (p (segment->list (make-segment dat rest))
	 (lambda ()			;Try a longer segment, if possible.
	   (if (null? rest) (fail) (try-segment (cdr rest))))
	 (lambda (result fail)		;Proposed segment is acceptable
	   (succeed-with-rest
	    rest 
	    (extend-dictionary vname (make-segment dat rest) result dict)
	    fail))))
    (let ((e (lookup vname dict)))
      (if e				; If the segment variable is already 
	  ;; bound, its value must be the segment at the head of the data
	  ;; and the match must proceed with the rest of the data.
	  (compare-segment-to-list-head
	   (matcher-entry-data e) dat
	   fail
	   (lambda (rest-list fail)
	     (succeed-with-rest rest-list dict fail)))

	  ;; Otherwise, try matching the segment against successively
	  ;; longer initial segments of the data.  TRY-SEGMENT tries an
	  ;; initial segment.  If it fails, it tries a longer segment.
	  ;; If it succeeds, it calls SUCCEED-WITH-REST, but with a
	  ;; failure continuation that will try a longer segment if we
	  ;; need to backtrack to this point.
	  (try-segment dat)))))


;;; Compare a segment with an initial segment of data.  If they are equal,
;;; succeed with the rest of the data.  Otherwise fail.

(define (compare-segment-to-list-head segment lst fail succeed)
  (let ((endseg (segment-end segment)))
    (define (scan seg-ptr rest-lst)
      (cond ((eq? seg-ptr endseg)
	     (succeed rest-lst fail))
            ((null? rest-lst) (fail))
	    ((equal? (car seg-ptr) (car rest-lst))
             (scan (cdr seg-ptr) (cdr rest-lst)))
	    (else (fail))))
    (scan (segment-start segment) lst)))

;;; Syntax of the data being manipulated:

(define (constant? exp) (not (pair? exp)))
(define same-constant? equal?)


;;; Rule syntax

(define (make-rule pat proc) (list pat proc))
(define (rule-pattern rule) (car rule))
(define (rule-procedure rule) (cadr rule))


;;; Pattern syntax.

(define (arbitrary-element? pat)
  (and (pair? pat) (eq? (car pat) '?)))

(define (start-arbitrary-segment? pat)
  (and (pair? pat) (pair? (car pat)) (eq? (caar pat) '??)))

(define (var-name pat)
  (cadr pat))

(define (var-restriction pat)
  (if (null? (cddr pat))
      always-succeed
      (caddr pat)))

(define (always-succeed dat fail succeed)
  (succeed dat fail))


;;; Segments

(define (make-segment start end)
  (list '*segment* start end))

(define (segment? x)
  (and (pair? x)
       (eq? (car x) '*segment*)))

(define segment-start cadr)
(define segment-end caddr)

(define (segment->list segment)
  (let ((end (segment-end segment)))
    (define (collect-segment pointer)
      (if (eq? pointer end)
          '()
          (cons (car pointer)
                (collect-segment (cdr pointer)))))
    (collect-segment (segment-start segment))))

(define (restrict-segment proc seg)
  (proc (segment->list seg)))
  

(define (convert-matcher-entry val)
  (if (segment? val)
      (segment->list val)
      val))


;;; Dictionaries

(define (make-empty-dictionary) '())

(define (make-matcher-entry name data info)
  (list name data info))

(define matcher-entry-name car)
(define matcher-entry-data cadr)
(define matcher-entry-info caddr)

(define lookup assq)

(define (extend-dictionary name data info dict)
  (cons (make-matcher-entry name data info) dict))

(define (dictionary-info name dict)
  (let ((e (lookup name dict)))
    (if e
	(matcher-entry-info e)
	(error "name not in dictionary" name))))

(define (matched-expression name dict)
  (let ((e (lookup name dict)))
    (if e
	(matcher-entry-data e)
	(error "name not in dictionary" name))))


(define (value patvar dict)
  (convert-matcher-entry (dictionary-info patvar dict)))
