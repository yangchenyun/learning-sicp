;;;;SMFRESH.SCM   Code for Sample Pset
;;; Routines for generating fresh symbols -- the Submodel version of gensym
;;; Bletcherous string and hash table hacking---NOT for students

                            ;;FOR THE COMPILER

(declare (usual-integrations))
(declare (integrate-operator inc))
(declare (integrate-operator dec))
(declare (integrate-operator table-entry))
(declare (integrate-operator get-fresh))
(declare (integrate-operator split-off-suffix))
(declare (integrate-operator make-suffixed-symbol))
(declare (integrate-operator enter-variable-into-table!))
(declare (integrate-operator delete-suffix-number))
(declare (integrate-operator get-cleaned-variable))
         

(define inc 1+)
(define dec -1+)

(load-option 'hash-table)

(define sm-symbol-table (make-symbol-hash-table)) ;associations of the form 
                                        ;KEY: var#n   DATUM: var#(n + 1)

(define (clean!) (hash-table/clean! sm-symbol-table))

(define (clear!) (hash-table/clear! sm-symbol-table))

(define (show-keys) (pp (hash-table/key-list sm-symbol-table)))

(define (table-entry var)
  (hash-table/get sm-symbol-table var #f))

;;GET-FRESH: (<symbol> + Empty) --> <symbol>

(define get-fresh
  (lambda var
    (let ((var (if (null? var)
                   'no-name-supplied
                   (car var))))
      ((split-off-suffix var)
       (lambda (var-string suffix-num)
         (let ((var-with-zero-suffix 
                (make-suffixed-symbol var-string 0)))
           (define (search-for-fresh current-suffixed-var)
             (let ((next-var (table-entry current-suffixed-var)))
               (if next-var
                   (search-for-fresh next-var)
                   current-suffixed-var)))
           (let ((freshvar (search-for-fresh var-with-zero-suffix)))
             (enter-variable-into-table! freshvar)
             freshvar)))))))

;; SPLIT-OFF-SUFFIX: symbol --> (((string,(Sch-Nonneg-Int + {'no-suffix})) --> T) --> T)

;; Examples:
;;   ((split-off-suffix 'x#4) c)  =  (c x 4)
;;   ((split-off-suffix 'x) c)    =  (c x 'no-suffix)
;;   ((split-off-suffix 'x#a) c)  =  (c x#a 'no-suffix)
;;   ((split-off-suffix 'x#-1) c) =  (c x#-1 'no-suffix)

(define (split-off-suffix var)
  (lambda (string-number-receiver)
    (let* ((var-string (symbol->string var))
           (lngth (string-length var-string)))
      (define (get-index n)
        (let ((next-char (string-ref var-string n)))
          (cond ((char-numeric? next-char)
                 (get-index (-1+ n)))
                ((char=? next-char #\#)    ;is NEXT-CHAR = # ?
                 (1+ n))
                (else lngth))))
      (let ((index (get-index (-1+ lngth))))
        (if (= index lngth)
            (string-number-receiver var-string 'no-suffix)
            (string-number-receiver
             (substring var-string 0 (-1+ index))
             (string->number (substring var-string index lngth))))))))
  
;;MAKE-SUFFIXED-SYMBOL: (string,(Sch-Nonneg-Int + {'no-suffix})) --> symbol
;;undoes SPLIT-OFF-SUFFIX, namely,
;;            ((SPLIT-OFF-SUFFIX 'x#3) MAKE-SUFFIXED-SYMBOL) = 'x#3
;;            ((SPLIT-OFF-SUFFIX 'x) MAKE-SUFFIXED-SYMBOL) = 'x

(define (make-suffixed-symbol root suffix)
  (if (eq? suffix 'no-suffix)
      (string->symbol root))
  (string->symbol 
   (string-append 
    root
    "#" 
    (number->string suffix))))

(define (enter-variable-into-table! var)
  ((split-off-suffix var)
   (lambda (var-string suffix-num)
     (hash-table/put!
      sm-symbol-table
      var
      (make-suffixed-symbol var-string
                            (+ 1 suffix-num))))))

(define (learn-variables! vars)
  (map enter-variable-into-table vars))

(define gensym generate-uninterned-symbol)

;;DELETE-SUFFIX-NUMBER: symbol --> string
;; (delete-suffix-number 'x#4)  =  'x
;;   (delete-suffix-number 'x)  =  'x


(define (delete-suffix-number var)
  ((split-off-suffix var)
   (lambda (var-string suffix-num)
     var-string)))

;;GET-CLEANED-VARIABLE: (<variable>, List(<variable>) --> <variable>

;(get-cleaned-variable prefix#n vars-to-avoid)
;;returns a variable not in vars-to-avoid, of the form "prefix" or "prefix#m"
;;with smallest possible m, e.g.,

;(get-cleaned-variable 'x#2 '(x y z#4 x#0))
;Value: x#1

;(get-cleaned-variable 'x#2 '(x#2 y z#4 x#0))
;Value: x

(define (get-cleaned-variable var vars-to-avoid)
  (let ((var-string (delete-suffix-number var))
        (split-vars-to-avoid (map (lambda (var)
                                    ((split-off-suffix var) cons))
                                  vars-to-avoid)))
    (define (search-for-cleaned suffix)
      (if (member (cons var-string suffix) split-vars-to-avoid)
          (search-for-cleaned (+ 1 suffix))
          suffix))
    (if (member (cons var-string 'no-suffix) split-vars-to-avoid)
        (make-suffixed-symbol var-string (search-for-cleaned 0))
        (string->symbol var-string))))

