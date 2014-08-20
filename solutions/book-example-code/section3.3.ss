#lang r5rs
(require r5rs)

(let ((x '((a b) c d))
      (y '(e f)))
  (set-car! x y) x)

;; the cons can even be defined as `set-car!' and `set-cdr!'
(define (get-new-pair) '()) ;; a naive implementation

;; (define (cons x y)
;;   (let ((new (get-new-pair)))
;;     (set-car! new x)
;;     (set-cdr! new y)
;;     new))

;; sharing and identify
(define x '(a b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b)
                 (list 'a 'b)))
z1
z2

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)

;; distinguish list components with eq?
(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;; mutable data could be implemented as procedure as well
(define old-cons cons)
(define (cons x y)
  (define (dispatch m)
    (cond
     ((eq? m 'car) x)
     ((eq? m 'cdr) y)
     ((eq? m 'set-car!) (lambda (v) (set! x v)))
     ((eq? m 'set-cdr!) (lambda (v) (set! y v)))
     (else
      (error "Undefined operation: CONS" m))))
  dispatch)

(let ((car (lambda (z) (z 'car)))
      (cdr (lambda (z) (z 'cdr)))
      (set-car! (lambda (z v)
                  ((z 'set-car!) v)
                  z))
      (set-cdr! (lambda (z v)
                  ((z 'set-cdr!) v)
                  z)))

  (define z (cons 'a 'b))
  (car z)
  (cdr z)
  (set-car! z 1))

(define cons old-cons)

;; some new data structure based on mutable pairs
;; queue

(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (make-queue)
  (cons '() '()))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "cannot get the front of an empty queue")
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? q)
        (begin
          (set-front-ptr! q new-pair)
          (set-rear-ptr! q new-pair)
          q)
        (begin
          (set-cdr! (rear-ptr q) new-pair)
          (set-rear-ptr! q new-pair)
          q))))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "cannot delete on an empty queue")
      (begin
        (set-front-ptr! q (cdr (front-ptr q)))
        q)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)
(insert-queue! q 'c)
(insert-queue! q 'd)
(delete-queue! q)


;; build the table data structure
;; one-dimensional table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond
   ((null? records) #f)
   ((eq? key (caar records)) (car records))
   (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (cons '*table* '()))

(define t (make-table))

;; two-dimensional table
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons
                   (cons key-1
                         (list (cons key-2 value)))
                   (cdr table)))))
  'ok)

(define t (make-table))

(insert! 'math '+ 43 t)
(insert! 'math '- 45 t)
(insert! 'math '* 42 t)
(insert! 'letter 'a 97 t)

(lookup 'letter 'a t)
(lookup 'math '+ t)

;; Simulator for Digital Circuits
;; wires which holds digital signals
;; function boxes connect wires carrying signals to other wires
;; delay, the output is delayed at a time

;; wire are represented by objects
;; (define a (make-wire))
;; (define b (make-wire))
;; (define c (make-wire))
;; (define d (make-wire))
;; (define e (make-wire))
;; (define s (make-wire))

;; function boxes are represented by procedures
;; (or-gate a b d)
;; (and-gate a b c)
;; (and-gate d e s)
;; (inverter c e)

;; abstract circuit could be constructed as procedures as well
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (and-gate d e s)
    (inverter c e)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; signals are represented as internal state of wire
;; a selector, mutator and a listener
;; a `after-delay' to run a procedure in the future

(define (logical-not s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (else (error "Invalid signal" s))))

(define (logical-and a1 a2)
  (cond ((and (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 1)) 0)
        ((and (= a1 1) (= a2 0)) 0)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "Invalid signal" s))))

(define (logical-or a1 a2)
  (cond ((and (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 1)) 1)
        ((and (= a1 1) (= a2 0)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

(define (make-wire)
  (let ((signal-value 0)
        (action-procs '()))
    (define (set-my-signal! value)
      (if (not (= signal-value value))
          (begin
            (set! signal-value value)
            (call-each action-procs))
          'done))
    (define (accept-action-proc! proc)
      (begin
        (set! action-procs (cons proc action-procs))
        (proc)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin
        ((car procs))
        (call-each (cdr procs)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

;; maintain a explicit agenda for `after-delay' procedure
(define (make-agenda) (list 0))
(define *the-agenda* (make-agenda))
(define (reset-the-agenda) (set! *the-agenda* (make-agenda)))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
                  action
                  *the-agenda*))

(define (propagate)
  (if (empty-agenda? *the-agenda*)
      'done
      (let ((first-item (first-agenda-item *the-agenda*)))
        (sleep .1)
        (first-item)
        (remove-first-agenda-item! *the-agenda*)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time *the-agenda*))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;; the agenda
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))



(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! (cdr segments))))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define or-gate-delay 5)
(define and-gate-delay 3)
(define inverter-delay 2)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(reset-the-agenda)

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)

;; 3.3.5 Constrains
;; concepts:
;; primitive constraints: certain relationship holds between quantities
;; connectors: holds a value which may participate in more constrains
;; constrain networks: a network of connectors and constrains

;; as the wire before connect holds an value
;; and list of constrains it is connected to
(define (make-connector)
  (let ((value #f)
        (resolved #f)
        (action-procs '()))

    (define (accept-action! proc)
      (set! action-procs
            (cons proc action-procs)))

    (define (set-value! new-value)
      (cond ((not (has-value? self))
             (begin
               (set! value new-value)
               (set! resolved #t)
               (call-each action-procs)))
            ((not (= new-value value))
             (error "Contradiction" (list value new-value)))
            (else 'ignored)))

    (define (forget-value!)
      (begin
        (set! value #f)
        (set! resolved #f)
        'done))

    (define (self m)
      (cond
       ((eq? m 'get-value) value)
       ((eq? m 'add-action!) accept-action!)
       ((eq? m 'set-value!) set-value!)
       ((eq? m 'forget-value!) (forget-value!))
       ((eq? m 'has-value?) resolved)
       (else (error "Unknown method for connector" m))))
    self))

;; helper methods
(define (get-value connector)
  (connector 'get-value))
(define (forget-value! connector)
  (connector 'forget-value!))
(define (has-value? connector)
  (connector 'has-value?))
(define (set-value! connector new-value)
  ((connector 'set-value!) new-value))
(define (add-action! connector action)
  ((connector 'add-action!) action))

;; define constrains, it must accept the 'wakeup message and
;; perform "proper" operation to fulfill the constrains
;; if any value is updated, it should be

(define (resolved-count connectors)
  (define (iter l c)
    (if (null? l)
        c
        (iter (cdr l)
              (+ c (if (car l) 1 0)))))
  (iter (map has-value? connectors) 0))

;; add a listener to all other-connectors
;; apply the value of other-connectors to the proc
;; when and only when all the other-connectors are fulfilled
(define (enforce-relation connector proc . others)
  (define (apply-when-resolved)
    (let ((resolved-res
           (if (= (length others)
                  (resolved-count others))
               (apply proc (map get-value others))
               #f)))
      (if resolved-res
          (set-value! connector resolved-res))))
  (for-each (lambda (other)
              (add-action! other apply-when-resolved)) others))

;; the equation should be mapped as dynamic relations
(define (adder a1 a2 sum)
  (enforce-relation a1 (lambda (a2 sum)
                         (- sum a2)) a2 sum)
  (enforce-relation a2 (lambda (a1 sum)
                         (- sum a1)) a1 sum)
  (enforce-relation sum (lambda (a1 a2)
                          (+ a1 a2)) a1 a2))

(define (multiplier m1 m2 product)
  (enforce-relation m1 (lambda (m2 product)
                         (/ product m2)) m2 product)
  (enforce-relation m2 (lambda (m1 product)
                         (/ product m1)) m1 product)
  (enforce-relation product (lambda (m1 m2)
                          (* m1 m2)) m1 m2))

(define (constant value connector)
  (set-value! connector value))

(define (probe-connector name connector)
  (add-action! connector
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display " New-value = ")
                 (display (get-value connector)))))

;; a little tests over constrains
(define a (make-connector))
(define b (make-connector))
(define s (make-connector))
(probe-connector 'a a)
(probe-connector 'b b)
(probe-connector 's s)
(adder a b s)

(set-value! a 3.14)
(set-value! b 0.5)
;; s New-value = 7

(define m (make-connector))
(define n (make-connector))
(define p (make-connector))
(probe-connector 'm m)
(probe-connector 'n n)
(probe-connector 'p p)
(multiplier m n p)

(set-value! m 3.14)
;; m New-value = 3
(set-value! n 4.5)
;; n New-value = 4
;; p New-value = 12

;; now test against our system
(define (celsius-fahrenheit-converter C F)
  (let
      ((w (make-connector))
       (u (make-connector))
       (v (make-connector))
       (x (make-connector))
       (y (make-connector)))

    (multiplier C w u) ;; C*w = u
    (multiplier v x u) ;; v*x = u
    (adder v y F) ;; F + y = v
    (constant 5 x)
    (constant 32 y)
    (constant 9 w)
    'ok))

(define C (make-connector))
(define F (make-connector))
(constant 25 C)
(celsius-fahrenheit-converter C F)

(get-value F) ;; should return the correct results, 77

;; some thoughts with comparison of implementation in the book

;; I tried to abstract a message system between connector and
;; constrain but I found there will be a lot handling for equations
;; and I choose to treat constrain as an enforcer of relationship
;; between connectors and all the relation happened directly between
;; connectors

;; I also use the time scheduling model from last example, but it seems
;; could be avoided at all

;; I also missing the ability to checking the value to be set
;; and the avoid loop notification
