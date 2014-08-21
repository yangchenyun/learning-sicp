;; the book's implementation
(define (remove-mem elem list)
    (cond ((null? list) '())
          ((eq? elem (car list))
           (remove-mem elem (cdr list)))
          (else
           (cons (car list)
                 (remove-mem elem (cdr list))))))

(define (map-except exclude proc list)
  (map proc (remove-mem exclude list)))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-self-value! new-value setter)
      (cond
       ((not (has-value? self))
        (begin
          (set! value new-value)
          (set! informant setter)
          (map-except setter
                      inform-about-value
                      constraints)
          'done))
       ((not (eq? value new-value))
        (error "Contradicts: " (list value new-value)))
       (else 'ignored)))

    (define (forget-self-value! retractor)
      (if (eq? informant retractor)
          (begin
            (set! informant #f)
            (map-except retractor
                        inform-about-no-value
                        constraints))
          'ignored))

    (define (connect-constraint new-constraint)
      (if (memq new-constraint constraints)
          'ignored
          (set! constraints
                (cons new-constraint constraints)))
      ;; enforce the constrains
      (if (has-value? self)
          (inform-about-value new-constraint)))

    (define (self m)
      (cond
       ((eq? m 'set-value!) set-self-value!)
       ((eq? m 'forget-value!) forget-self-value!)
       ((eq? m 'has-value?) (if informant #t #f))
       ((eq? m 'get-value) value)
       ((eq? m 'connect) connect-constraint)
       (else (error "Unknown operation: CONNECTOR" m))))
    self))

(define (set-value! connector new-value setter)
  ((connector 'set-value!) new-value setter))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'get-value))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (adder a1 a2 sum)

  (define (process-new-value)
    (cond
     ((and (has-value? a1)
           (has-value? a2))
      (set-value! sum (+ (get-value a1) (get-value a2)) self))
     ((and (has-value? a1)
           (has-value? sum))
      (set-value! a2 (- (get-value sum) (get-value a1)) self))
     ((and (has-value? a2)
           (has-value? sum))
      (set-value! a1 (- (get-value sum) (get-value a2)) self))))

  (define (process-forget-value)
    (forget-value! sum self)
    (forget-value! a1 self)
    (forget-value! a2 self)
    (process-new-value))

  (define (self m)
    (cond
     ((eq? m 'new-value) (process-new-value))
     ((eq? m 'no-value) (process-forget-value))
     (else (error "Unknown request: Adder" m))))
  (connect a1 self)
  (connect a2 self)
  (connect sum self)
  self)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1)
                    (= (get-value m1) 0))
               (and (has-value? m2)
                    (= (get-value m2) 0)))
           (set-value! product 0 self))
          ((and (has-value? m1)
                (has-value? m2))
           (set-value! product
                       (* (get-value m1)
                          (get-value m2))
                       self))
          ((and (has-value? product)
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       self))
          ((and (has-value? product)
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       self))))
  (define (process-forget-value)
    (forget-value! product self)
    (forget-value! m1 self)
    (forget-value! m2 self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'new-value)
           (process-new-value))
          ((eq? request 'no-value)
           (process-forget-value))
          (else
           (error "Unknown request:
                   MULTIPLIER"
                  request))))
  (connect m1 self)
  (connect m2 self)
  (connect product self)
  self)

(define (constant value connector)
  (define (self m)
    (error "Unknown request: CONSTANT" m))
  (connect connector self)
  (set-value! connector value self)
  self)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (self request)
    (cond ((eq? request 'new-value)
           (process-new-value))
          ((eq? request 'no-value)
           (process-forget-value))
          (else (error "Unknown request:
                        PROBE" request))))
  (connect connector self)
  self)

(define (inform-about-no-value constraint)
  (constraint 'no-value))

(define (inform-about-value constraint)
  (constraint 'new-value))

(define (new-celsius-fahrenheit-converter C F)
  (let ((w (make-connector))
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

;; (define C (make-connector))
;; (define F (make-connector))
;; (probe "Celsius temp" C)
;; (probe "Fahrenheit temp" F)
;; (new-celsius-fahrenheit-converter C F)
;; (set-value! C 25 'user)
;; (set-value! F 212 'user)
;; (forget-value! C 'user)
