#lang r5rs
(require racket/include)
(include "../lib/arithmetic.ss")

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define x '((1 2) 3 4))
(count-leaves x)

;; two procedures shared a similar signal-processing pattern
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; redefine the procedure to conceptualize these processes of 'signals flowing'

;; helper methods to process signal
(define (filter p sequence)
  (cond ((null? sequence) '())
        ((p (car sequence))
         (cons (car sequence) (filter p (cdr sequence))))
        (else (filter p (cdr sequence)))))

(define (accumulate op init sequence)
  (if (null? sequence) init
      (op (car sequence) (accumulate op init (cdr sequence)))))

(define (enumerate-interval l h)
  (if (> l h)
      '()
      (cons l (enumerate-interval (+ l 1) h))))

(define (atom? x) (not (pair? x)))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

;; now redefine the two procedures

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs 10)

(let ((n 10))
  (accumulate append '() (map (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n))))

(let ((n 10))
  (accumulate append '() (map (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n))))

;; a commonly used procedure is abstracted
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; now the procedure to generate integer pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime? n)
  (define (test div)
    (cond ((> (square div) n) #t)
          ((= 0 (remainder n div)) #f)
          (else (test (if (even? div) (+ div 1) (+ div 2))))))
  (test 2))

(define (pair-sum pair) (+ (car pair) (cadr pair)))
(define (prime-sum? pair) (prime? (pair-sum pair)))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (pair-sum pair)))

;; now the final procedure to finish the problem
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 6)

;; now procedure to generate permutations
(define (permutations s)
  (if (null? s)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (subset)
                        (cons x subset))
                      (permutations (remove x s))))
               s)))

(permutations '(1 2 3))
