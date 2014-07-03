;;; PART 2
;;; test simple expressions
-37

(* 3 4)

(> 10 9.7)

(- (if (> 3 4)
       7
       10)
   (/ 16 10))

(* (- 25 10)
   (+ 6 3))

+

(define double (lambda (x)
                 (* 2 x)))

double

(define c 4)

c

(double c)

c

(double (double (+ c 5)))

(define times-2 double)

(times-2 c)

(define d c)

(= c d)

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (- c d)))

;;; PART 3 Pretty printing
(define abs
  (lambda (a)
    (if (> a 0)
        a
        (- a))))

;;; PART 5 Documentation and Administrative Question
1. stepper is an interactive shell to scheme in which you could
evaluate the program expression by expression.
Debugger is ...

2. the (define d 1) expression, (cond ...) expression.

6. abstraction, conventional interfaces, metalinguistic
;; Abstraction
Abstraction means hiding the details of implementation to users.
the procedure square could be implemented in two ways, but for the user of
the details are hidden.
It is a method of abstraction for the procedure square.
(define (square x) (* x x))
(define (square x) (exp (+ (log x) (log x))))

;; conventional interfaces
For the procedure +, it should be able to be applied to numbers, strings,
vectors, electronic waves etc.
To make the procedure + works on those data, we need a unified interface
to apply the + procedure.
This method makes our program extendable and consistent.

;; meta-linguistic
I could only come up with the examples I see in the real world.
In sinatra, it comes with its own Domain-Specific-Language with
keywords get, post, delete, put maps to HTTP method.

7. scheme is case-insensitive
