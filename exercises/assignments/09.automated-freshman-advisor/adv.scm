;;;; This is the file adv.scm
;;;; It implements the freshman advisor

;;; Top-level procedure

(define (see-advisor name)
  (write-line (list 'hi name))
  (write-line '(i am your freshman advisor))
  (write-line '(what are your plans for the semester))
  (advisor-driver-loop name))

;;; Driver loop

(define (advisor-driver-loop name)
  (let ((user-response (prompt-for-command-expression "** ")))
    (cond ((equal? user-response '(goodbye))
	   (write-line (list 'goodbye name))
	   (write-line '(have a good semester!)))
	  (else (reply-to user-response)
		(advisor-driver-loop name)))))


;;; Select method for generating the reply

(define (reply-to input)
  (cond ((translate-and-run input subject-knowledge))
	((translate-and-run input conventional-wisdom))
	((with-odds 1 2)
	 (write-line (reflect-input input)))
	(else
	 (write-line (pick-random general-advice)))))


;;; First, let's do the simple, stupid things.

;;; This returns true n1 out of n2 times
(define (with-odds n1 n2)
  (< (random n2) n1))

;;; One simple response method is to repeat the user's reponse, after 
;;; changing first-person words to second person

(define (reflect-input input)
  (append (pick-random beginnings)
	  (change-person input)))


(define (pick-random list)
  (list-ref list (random (length list))))

(define beginnings
  '((you say)
    (why do you say)
    (i am glad to hear that)
    ()))

(define (change-person phrase)
  (sublist '((i you) (me you) (am are) (my your))
	   phrase))

(define (sublist replacements list)
  (map (lambda (elt) (substitute replacements elt)) list))

(define (substitute replacements item)
  (cond ((null? replacements) item)
	((eq? item (caar replacements)) (cadar replacements))
	(else (substitute (cdr replacements) item))))

;;; Another simple response method is to just reply with with some
;;; general advice chosen at random

(define general-advice
  '((make sure to take some humanities)
    (mit has a lot of interesting departments)
    (make sure to get time to explore the Boston area)
    (how about a freshman seminar)))

;;; More sophisticated methods match the input against a set of rules.
;;; Translate-and-run will try the rules, and return FALSE if no rules
;;; match.  If some rule matched, then the rule action is run, and
;;; translate-and-run returns TRUE.

(define (translate-and-run input rules)
  (try-rules input rules
	     (lambda () false)	            
	     (lambda (result fail) true)))  


;;; The simplest rules just test for patterns in the input, and print
;;; some conventional wisdom.

;;; this procedure ignores the match dictionary and just prints the response
(define (simple-response text)
  (lambda (dict) (write-line text)))

(define conventional-wisdom
  (list
   (make-rule
    '((?? x) 6:001 (?? y))
    (simple-response '(6:001 is too much work for freshmen
			     -- wait until next year)))
   (make-rule
    '((?? x) 8:01 (?? y))
    (simple-response '(students really enjoy 8:01)))
   (make-rule
    '((?? x) seminar (?? y))
    (simple-response '(i hear that snorkeling in Boston Harbor
			 is a really exciting seminar)))

   (make-rule
    '((?? x) to take (?? y) next (?? z))
    (lambda (dict)
      (write-line
       (append '(too bad -- )
	       (value 'y dict)
	       '(is not offered next)
	       (value 'z dict)))))
   (make-rule
    '((?? x) double major in (?? y) and (?? z))
    (lambda (dict)
      (write-line
       (append
	(value 'y dict)
	'(is fascinating and you can make a living doing it if)
	(value 'z dict)
	'(does not work out)))))
   (make-rule
    '((?? x) double major (?? y))
    (simple-response '(doing a double major is a lot of work)))
    ))

;;; More sophisticated responses depend on knowledge drawn from the
;;; MIT catalog.

;;; Here is a simple catalog data structure with a few entries:

(define (make-entry subject department summary units satisfies prerequisites)
  (list subject department summary units satisfies prerequisites))

(define (entry-subject entry) (list-ref entry 0))
(define (entry-department entry) (list-ref entry 1))
(define (entry-summary entry) (list-ref entry 2))
(define (entry-units entry) (list-ref entry 3))
(define (entry-satisfies entry) (list-ref entry 4))
(define (entry-prerequisites entry) (list-ref entry 5))


(define catalog
  (list 
   (make-entry '8:01 'physics '(classical mechanics) 12 '(GIR physics) '())
   (make-entry '8:02 'physics '(electricity and magnetism)
	    12 '(GIR physics) '(8:01 18:01))
   (make-entry '8:03 'physics '(waves) 12 '(rest) '(8:02 18:02))
   (make-entry '8:04 'physics '(quantum wierdness) 12 '(REST) '(8:03 18:03))
   (make-entry '18:01 'math '(elementary differential and integral calculus)
	    12 '(GIR calculus) '())
   (make-entry '18:02 'math '(multivariate calculus) 
	    12 '(GIR calculus) '(18:01)) 
   (make-entry '18:03 'math '(differential equations) 12 '(REST) '(18:02))
   (make-entry '18:04 'math '(theory of functions of a complex variable)
	    12 '() '(18:03))
   (make-entry '6:001 'eecs '(scheming with abelson and sussman)
	    15 '(REST) '(true-grit))
   (make-entry '6:002 'eecs '(circuits) 15 '(REST) '(8:02 18:02))
   (make-entry '3:091 'mms '(munching and crunching stuff)
	    12 '(GIR chemistry) '())
   (make-entry '5:11 'chemistry '(smelly organic crud and goop)
	    12 '(GIR chemistry) '(a-strong-stomach))
   (make-entry '7:012 'biology '(diseases and their applications)
	    12 '(GIR biology) '())
   (make-entry '7:013 'biology '(diseases and their applications)
	    12 '(GIR biology) '())
   (make-entry '7:014 'biology '(diseases and their applications)
	    12 '(GIR biology) '())
   (make-entry '12:001 'eaps '(rocks for jocks) 12 '(REST) '())
   (make-entry '12:004 'eaps '(planets) 12 '() '(18:03 8:02)) ))
   

;;;Using the catalog, we can define some match restrictions

;;;Given a subject, succeeds with the catalog entry for that subject if it
;;;is in the catalog, oterwise fails

(define (in-catalog subject fail succeed)
  (let ((entry (find subject catalog)))
    (if entry
	(succeed entry fail)
	(fail))))


;;;Matches a list of subjects of the form s1 s2 .... and sk

(define (subjects words fail succeed)
  (try-rules
   words
   (list
    (make-rule
     `((? subject ,in-catalog))
     (lambda (dict)
       (list (value 'subject dict))) )
    (make-rule
     `((?? list-of-subjects ,subject-seq) and (? final-subject ,in-catalog))
     (lambda (dict)
       (append (value 'list-of-subjects dict)
	       (list (value 'final-subject dict)))) )
     )
   fail
   succeed))


;;; Subprocedure used by SUBJECTS.  Matches a sequence of subjects 
;;; s1 s2 ... sn

(define (subject-seq words fail succeed)
  (try-rules
   words
   (list
    (make-rule
     `((? subject ,in-catalog))
     (lambda (dict)
       (list (value 'subject dict))) )
    (make-rule
     `((? subject1 ,in-catalog) (?? more-subjects ,subject-seq))
     (lambda (dict)
       (cons (value 'subject1 dict) (value 'more-subjects dict))) )
     )
   fail
   succeed))
	     

;;; Now we can define some actual reponses based upon catalog knowledge

(define subject-knowledge 
  (list
   (make-rule
    `(what is (? s ,in-catalog) about)
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append (list (entry-subject entry))
		 '(is about)
		 (entry-summary entry))))) )

   (make-rule
    `(what are (?? s ,subjects) about)
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append (list (entry-subject entry))
			   '(is about)
			   (entry-summary entry))))
		(value 's dict))) )
    
   (make-rule
    `(how many units is (? s ,in-catalog))
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append (list (entry-subject entry))
		 '(is a)
		 (list (entry-units entry))
		 '(unit subject))))) )

   (make-rule
    `(how many units are (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append (list (entry-subject entry))
			   '(is a)
			   (list (entry-units entry))
			   '(unit subject))))
		(value 's dict))) )

   (make-rule
    `(what are the prerequisites for (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append '(the prerequisites for)
			   (list (entry-subject entry))
			   '(are)
			   (entry-prerequisites entry))))
		(value 's dict))) )

   (make-rule
    `(can I take (? s ,in-catalog))
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append '(the prerequisites for)
		 (list (entry-subject entry))
		 '(are)
		 (entry-prerequisites entry))))) )

   ))


(define (filter test? subjects)
  (cond ((null? subjects) '())
	((test? (car subjects))
	 (cons (car subjects)
	       (filter test? (cdr subjects))))
	(else 
	  (filter test? (cdr subjects)))))


(define (find subject entries)
  (cond ((null? entries) false)
	((eq? subject (entry-subject (car entries)))
	 (car entries))
	(else
	 (find subject (cdr entries)))))

(define (list-union l1 l2)
  (cond ((null? l1) l2)
	((member (car l1) l2)
	 (list-union (cdr l1) l2))
	(else
	 (cons (car l1)
	       (list-union (cdr l1) l2)))))


(define (list-intersection l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (cons (car l1)
	       (list-intersection (cdr l1) l2)))
	(else (list-intersection (cdr l1) l2))))


(define (reduce combiner initial-value list)
  (define (loop list)
    (if (null? list)
	initial-value
	(combiner (car list) (loop (cdr list)))))
  (loop list))

