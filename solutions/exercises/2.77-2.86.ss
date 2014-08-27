#lang r5rs
(#%require racket/include)
(include "../lib/dispatch.ss")
(include "../lib/arithmetic.ss")
(include "../lib/utils.ss")

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;; Exercise 2.77
;; `magnitude' is exposed in the polar-angle complex package and it will
;; dispatch on the data with one data type prefix

;; for an object like z '(complex rectangular 3 4), there is no way for
;; the `magnitude' method to dispatch on 'complex symbol.

;; a detailed work through, assuming magnitude is exposed in the polar-package
;; (define (magnitude z) (apply-generic 'magnitude z))

;; (define z (make-complex-from-real-imag 3 4))
;; (magnitude z)
;; (apply-generic 'magnitude '(complex rectangular 3 . 4))
;; (get 'magnitude 'complex) ;; this would fail

;; However, after we install the selector methods in the complex-package
;; (put 'magnitude '(complex) magnitude)
;; (apply-generic 'magnitude '(complex rectangular 3 . 4))
;; (magnitude (map contents args))
;; (magnitude '(rectangular 3 . 4))
;; (apply-generic 'magnitude '(rectangular 3 . 4))
;; this will be successfully dispatched

;; Exercise 2.78
;; add another representation of datum in the system and
;; the operations should be dispatched (explicitly) according to the type

;; datum := (symbol . data) | data

;; symbol, data -> datum
(define (attach-tag type-tag contents)
  (if (or (symbol? contents)
          (number? contents))
      contents
      (cons type-tag contents)))

;; datum -> symbol
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((symbol? datum) 'scheme-symbol)
        ((number? datum) 'scheme-number)
      (error "Bad tagged datum: TYPE-TAG" datum)))

;; datum -> data
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((or (symbol? datum) (number? datum)) datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(make-scheme-number 10)

;; Exercise 2.79
(define (equ? a b)
  (apply-generic 'equ? a b))

(define (install-equ?-package)
  (define ordinary-equ? =)

  ;; rational packages
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (rational-equ? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

  (define (complex-equ? a b)
    (and
     (= 0 (- (real-part a) (real-part b)))
     (= 0 (- (imag-part a) (imag-part b)))))

  (put 'equ? '(scheme-number scheme-number) ordinary-equ?)
  (put 'equ? '(rational rational) rational-equ?)
  (put 'equ? '(complex complex) complex-equ?))

(install-equ?-package)

(equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))
(equ? (make-scheme-number 10) (make-scheme-number 10))
(equ? (make-rational 10 5) (make-rational 2 1))
(equ? (make-rational 10 5) (make-rational 3 1))

;; Exercise 2.80
(define (=zero? a)
  (apply-generic '=zero? a))

(define (install-=zero?-package)
  (define ordinary-=zero? zero?)

  ;; rational packages
  (define (numer x) (car x))
  (define (rational-=zero? x)
    (zero? (numer x)))

  (define (complex-=zero? a)
    (and
     (zero? (real-part a))
     (zero? (imag-part a))))

  (put '=zero? '(scheme-number) ordinary-=zero?)
  (put '=zero? '(rational) rational-=zero?)
  (put '=zero? '(complex) complex-=zero?))

(install-=zero?-package)

(=zero? (make-complex-from-real-imag 0 4))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-scheme-number 0))
(=zero? (make-rational 10 5))
(=zero? (make-rational 0 5))

;; Exercise 2.81
;; a.
;; it will result in an infinite loop.
;; more details about the application, start with the evaluation:
;; (exp '(complex rectangular 1 2) '(complex rectangular 2 3))
;; (apply-generic 'exp '(complex rectangular 1 2) '(complex rectangular 2 3))
;; because no proc is found for '(exp complex complex) in dispatch table

;; `apply-generic' will try coercion each arguments
;; the coercion '(complex complex) is existed in the coercion table so
;; this will be used to try another `apply-generic' application
;; (apply-generic 'exp (complex->complex '(complex rectangular 1 2)) '(complex rectangular 2 3))
;; (apply-generic 'exp '(complex rectangular 1 2) '(complex rectangular 2 3))

;; which is the exactly the original application. So it resulted in a infinite loop

;; b.
;; the original `apply-generic' works correctly, no identity coercion is needed.
;; the arguments will be coerced one by one until a possible procedure fits in
;; both types

;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tag)))
      (if proc
          (apply proc (map contents args))
          (if (= 2 (length args))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2) ;; this tests will stop coercion for same type
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond
                       (t1->t2 (apply-generic op (t1->t2 a1) a2))
                       (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                       (else
                        (error "No method for these types"
                               (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Exercise 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; now the coercion trials begin
          (letrec ((find-proc-for-coerce-args
                    (lambda (op args tags)
                      (if (null? tags)
                          (error "No method for these types" (list op type-tags))
                          ;; tried to find the proc if coercion on target-type is successful
                          (letrec ((target-type (car tags))
                                   (coerced-type-tags-if-success
                                    (map (lambda (arg) target-type) args))
                                   (proc-if-success (get op coerced-type-tags-if-success)))
                            (newline)
                            (display "trying to converting to: ")
                            (displayln target-type)
                            (display "the proc is: ")
                            (displayln proc-if-success)
                            (if proc-if-success
                                ;; now perform the real coercion
                                (let* ((coerced-args
                                        (map (lambda (arg)
                                               (let ((arg-type (type-tag arg)))
                                                 ;; doesn't coerce if already target-type
                                                 (if (eq? arg-type target-type)
                                                     arg
                                                     (let ((t (get-coercion
                                                               arg-type
                                                               target-type)))
                                                       (if t (t arg) 'no-co))
                                                     ))
                                               ) ;; signal for failure
                                             args))
                                       (coerced-success?
                                        (foldr (lambda (x r)
                                                 (and (not (eq? 'no-co x)) r)) #t coerced-args)))
                                  (displayln "coerced-arguments are: ")
                                  (displayln coerced-args)
                                  (displayln "successful coercion? ")
                                  (displayln coerced-success?)
                                  (if coerced-success?
                                      (apply proc-if-success (map contents coerced-args))
                                      (find-proc-for-coerce-args op args (cdr tags))))
                                ;; if proc-if-success failed, try next target type
                                (find-proc-for-coerce-args op args (cdr tags))))))))
            (find-proc-for-coerce-args op args type-tags))))))

;; test cases
(define (install-add+-package)
  (define (tag exp)
    (attach-tag 'complex exp))

  (define (add+ . operands)
    (let ((real-part-sum (foldr + 0 (map real-part operands)))
          (imag-part-sum (foldr + 0 (map imag-part operands))))
      (make-from-real-imag real-part-sum imag-part-sum)))

  (put 'add+ '(complex complex complex)
       (lambda (a b c) (tag (add+ a b c))))

  (put 'add+ '(complex complex complex complex)
       (lambda (a b c d) (tag (add+ a b c d))))
  'done)

(install-add+-package)
(define (add-3 a b c) (apply-generic 'add+ a b c))
(define (add-4 a b c d) (apply-generic 'add+ a b c d))

;; 3 arguments with different target-type
(add-3 (make-complex-from-real-imag 1 2) 2 3)
(add-3 2 (make-complex-from-real-imag 1 2) 3)
(add-3 1 2 (make-complex-from-real-imag 1 2))

;; test 4 arguments
(add-4 1 2 3 (make-complex-from-real-imag 1 2))

;; Exercise 2.83

;; redefine the tagging program, now number needed to
;; be distinguished from integer and real
(define (attach-tag type-tag contents)
  (if (symbol? contents)
      contents
      (cons type-tag contents)))

;; datum -> symbol
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((symbol? datum) 'scheme-symbol)
      (error "Bad tagged datum: TYPE-TAG" datum)))

;; datum -> data
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((symbol? datum) datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; integer -> rational -> real -> complex

;; global raise method
(define (raise n) (apply-generic 'raise n))

;; integer
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'raise '(integer)
       (lambda (x)
         (make-rational x 1)))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "MAKE: args is not integer" x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; rational
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (rational-raise r)
    (make-real
     (/ (numer r) (denom r))))

  (put 'raise '(rational) rational-raise)

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; real
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "MAKE: args is not real number" x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

;; complex is already installed elsewhere

(install-integer-package)
(install-rational-package)
(install-real-package)

(raise (make-integer 2))
(raise (raise (make-integer 2)))
(raise (raise (raise (make-integer 2))))
;; (raise (raise (raise (raise (make-integer 2)))))

;; Exercise 2.84
;; Algorithm: when the system is required to operate on objects of different types
;; it can successively raise the lower types until all the objects are at the same
;; level in the tower.

;; explicitly maintain the inheritance in a ordered set
(define (install-tower-package)
  (define TOWER '())
  (define (insert-before l old new)
    (if (or (null? l)
            (equal? (car l) old))
        (cons new l)
        (cons (car l) (insert-before (cdr l) old new))))
  (define (index-of el l)
    (cond
     ((null? l) #f)
     ((eq? el (car l)) 0)
     (else (+ 1 (index-of el (cdr l))))))
  (define (index-of-TOWER type)
    (index-of type TOWER))

  (define (add-subtype-of super sub)
    (set! TOWER
          (insert-before TOWER sub super))
    TOWER)

  ;; 'symbol, 'symbol -> 'sche-num
  ;; 0 if type1 = type2
  ;; 1 if type1 is supertype of type2
  ;; -1 if type1 is subtype of type2
  ;; error if type doesn't exist in TOWER
  (define (comp t1 t2)
    (let ((i1 (index-of-TOWER t1))
          (i2 (index-of-TOWER t2)))
      (if (and i1 i2)
          (- i1 i2)
          (error "type doesn't exist in tower" (list t1 t2)))))

  (put 'index-of-TOWER '(scheme-symbol) index-of-TOWER)
  (put 'comp '(scheme-symbol scheme-symbol) comp)
  (put 'add-subtype-of '(scheme-symbol scheme-symbol) add-subtype-of)
  ;; add to the root
  (put 'add-root-type '(scheme-symbol) (lambda (type)
                                          (add-subtype-of type '()))))

(begin
  (install-tower-package)
  (define (add-subtype-of super sub)
    ((get 'add-subtype-of '(scheme-symbol scheme-symbol)) super sub))
  (define (add-root-type type)
    ((get 'add-root-type '(scheme-symbol)) type))
  (define (comp t1 t2)
    ((get 'comp '(scheme-symbol scheme-symbol)) t1 t2))
  (define (index-of-TOWER type)
    ((get 'index-of-TOWER '(scheme-symbol)) type))

  ;; build the inheritance tree explicitly
  (add-root-type 'complex)
  (add-subtype-of 'real 'complex)
  (add-subtype-of 'rational 'real)
  (add-subtype-of 'integer 'rational))
;; Future addition of package only needed to `add-subtype-of' to inject type

(define (apply-generic op . args)
  (define (raise-arg-to arg target)
    (let ((current-type (type-tag arg)))
      (if (eq? current-type target)
          arg
          (raise-arg-to (raise arg) target))))

  (define (args-same-types? args)
    (let* ((types (map type-tag args))
          (first-type (car types)))
      (foldr (lambda (type same?)
               (and same? (eq? type first-type))
               ) #t types)))

  (define (highest-among types)
    (foldr (lambda (type current)
             (if (and (not (null? current))
                      (negative? (comp type current)))
                 current
                 type)
             ) '() types))

  (define (args-with-same-type args)
    (let ((type-tags (map type-tag args)))
     (if (args-same-types? args)
         args
         (let ((highest-type (highest-among type-tags)))
           (map (lambda (arg)
                  (raise-arg-to arg highest-type)) args)))))

  (let ((type-tags (map type-tag args)))
    (let ((same-type-args (args-with-same-type args)))
      (let ((proc (get op (map type-tag same-type-args))))
        (if proc
            (apply proc (map contents same-type-args))
            (error
             "No method for these types: APPLY-GENERIC"
             (list op type-tags)))))))

(add (make-integer 4) (make-rational 2 3))
(add (make-real 4.14) (make-rational 2 3))
(add (make-complex-from-real-imag 4 4.14) (make-rational 2 3))


;; Exercise 2.85
;; Begin by defining a generic operation project that “pushes” an object down in the tower.
;; when we project it and raise the result back, we end up with something equal to what we started with,
;; we can drop the

;; global raise method
;; integer
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'raise '(integer)
       (lambda (x)
         (make-rational x 1)))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "MAKE: args is not integer" x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; rational
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (rational-raise r)
    (make-real
     (/ (numer r) (denom r))))

  (put 'raise '(rational) rational-raise)
  (put 'project '(rational)
       (lambda (x)
         (make-integer (round (/ (numer x) (denom x))))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; real
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real)
       (lambda (x) (make-rational (round x) 1)))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "MAKE: args is not real number" x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

;; install complex projection
(put 'project '(complex)
     (lambda (x) (make-real (real-part x))))

(install-integer-package)
(install-rational-package)
(install-real-package)

(define (project n) (apply-generic 'project n))

(project (make-complex-from-real-imag 10 3))
(project (project (make-complex-from-real-imag 10 3)))
(project (project (project (make-complex-from-real-imag 10 3))))
(project (make-rational 3 2))

(define (drop n)
  (if (eq? 'integer (type-tag n))
      n
      (let ((after-project-and-raise (raise (project n))))
        (if (equ? n after-project-and-raise)
            (drop (project n))
            n))))

(drop (make-rational 2 1))

;; some tests
(drop (make-complex-from-real-imag 10 2))
(drop (make-complex-from-real-imag 10 0))
(drop (make-real 0.5))
(drop (make-rational 5 10))
(drop (make-rational 10 5))

;; don't use apply-generic in `project' and `raise',
;; because it will be called from `drop' in `apply-generic'
;; otherwise, it will cause infinite loop

;; Exercise 2.86
;; Changes in rectangular / polar package:
;; exposed methods are real-part, imag-part, magnitude, angle
;; `*', `+', `cos', `sin', `atan', `sqrt' needed to handle generic data types

;; Changes in the complex package:
;; arithmetic operations needed to substituted for generic operations

(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan x) (apply-generic 'arctan x))
(define (square-root x) (apply-generic 'square-root x))

(define (install-rectangular-package)
  ;; internal procedures
  ;; no name conflicts needed to be worried
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

;; now the selector could be defined without worrying about
;; pick the right representation of data
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; complex number now
(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; integer
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))

  (define (cosine x) (make-real (cos x)))
  (define (sine x) (make-real (sin x)))
  (define (arctan x) (make-real (atan x)))
  (define (square-root x) (make-real (sqrt x)))
  (put 'cosine '(integer) cosine)
  (put 'sine '(integer) sine)
  (put 'arctan '(integer) arctan)
  (put 'square-root '(integer) square-root)

  (put 'raise '(integer)
       (lambda (x)
         (make-rational x 1)))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "MAKE: args is not integer" x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; rational
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (rational-raise r)
    (make-real
     (/ (numer r) (denom r))))

  (define (cosine x) (make-real
                      (cos (/ (numer x) (denom x)))))
  (define (sine x) (make-real
                    (sin (/ (numer x) (denom x)))))
  (define (arctan x) (make-real
                      (atan (/ (numer x) (denom x)))))
  (define (square-root x) (make-real
                           (sqrt (/ (numer x) (denom x)))))
  (put 'cosine '(rational) cosine)
  (put 'sine '(rational) sine)
  (put 'arctan '(rational) arctan)
  (put 'square-root '(rational) square-root)

  (put 'raise '(rational) rational-raise)
  (put 'project '(rational)
       (lambda (x)
         (make-integer (round (/ (numer x) (denom x))))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; real
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (cosine x) (make-real (cos x)))
  (define (sine x) (make-real (sin x)))
  (define (arctan x) (make-real (atan x)))
  (define (square-root x) (make-real (sqrt x)))
  (put 'cosine '(real) cosine)
  (put 'sine '(real) sine)
  (put 'arctan '(real) arctan)
  (put 'square-root '(real) square-root)
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real)
       (lambda (x) (make-rational (round x) 1)))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "MAKE: args is not real number" x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-integer-package)
(install-rational-package)
(install-real-package)

;; test cases
(add
 (make-complex-from-real-imag (make-rational 1 2) (make-real 1.0))
 (make-complex-from-mag-ang (make-integer 100) (make-rational 1 6)))
