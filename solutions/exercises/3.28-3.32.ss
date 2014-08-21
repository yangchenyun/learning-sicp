#lang r5rs
(require r5rs)

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

;; Exercise 3.29
;; (or a b)
;; equals to (not (and (not a) (not b)))
(define (or-gate a1 a2 output)
  (let ((a1-inverted (make-wire))
        (a2-inverted (make-wire))
        (and-result (make-wire)))
    (inverter a1 a1-inverted)
    (inverter a2 a2-inverted)
    (and-gate a1-inverted a2-inverted and-result)
    (inverter and-result output)))

;; or-delay = 3 * inverter-delay + and-gate-delay

;; Exercise 3.30

;; helper functional methods
(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

(define (fold proc init . lists)
  (define (any-head-null? lists)
    (define (and-reduce list)
      (cond
       ((null? list) #t)
       ((car list) (and-reduce (cdr list)))
       (else #f)))
    (if (null? lists)
        #t
        (and-reduce (map null? lists))))

  (if (any-head-null? lists)
      init
      (let ((result (apply proc init (map car lists))))
        (apply fold proc result (map cdr lists)))))

(define (fold-right proc init . lists)
  (apply fold proc init (map reverse lists)))

;; http://d.hatena.ne.jp/tanakaBox/20080512/1210588399
(define (number->list n . base)
  (letrec ((b (if (null? base) 10 (car base)))
           (iter (lambda (acc n)
                   (let ((q (quotient n b))
                         (m (cons (modulo n b) acc)))
                        (if (zero? q)
                            m
                            (iter m q))))))
          (iter '() n)))

(define (list->number l . base)
  (letrec ((b (if (null? base) 10 (car base)))
           (iter (lambda (l cont)
                  (if (null? l)
                      (cont 0 0)
                      (iter (cdr l) (lambda (sum k)
                                      (cont (+ (* (car l) (expt b k)) sum)
                                            (+ k 1))))))))
          (iter l (lambda (sum k) sum))))

(define (number->wire n . base)
  (map (lambda (x)
         (let ((w (make-wire)))
           (set-signal! w x)
           w))
       (number->list n (car base))))

(define (ripple-carry-adder awires bwires)
  (define (align-binary-padding awires bwires)
    (if (>= (length awires) (length bwires))
        awires
        (align-binary-padding
         (cons (make-wire) awires) bwires)))

  (let ((res (fold-right (lambda (acc a b)
                     (let ((carry (make-wire))
                           (sum (make-wire)))
                       (full-adder a b (car acc) sum carry)
                       (cons carry
                             (cons sum (cdr acc)))))
                   (list (make-wire))
                   (align-binary-padding awires bwires)
                   (align-binary-padding bwires awires))))
    (propagate)
    res)) ;; keep the last carry

(let ((a (number->wire 2 2))
      (b (number->wire 8 2)))
  (list->number (map get-signal
                     (ripple-carry-adder a b)) 2))

;; a half-adder delay is or-gate + 2*and-gate + inverter
;; a full-adder delay is 2 * half-adder + or-gate
;;                    = 3*or-gate 4*and-gate + 2*inverter
;; so ripple-carry-adder delay for n-bit is: 3n*or-gate 4n*and-gate + 2n*inverter

;; Exercise 3.31
(define (half-addder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (and-gate d e s)
    (inverter c e)
    'ok))

;; when a half-adder runs, `add-action!' is called in the following sequence

;; or-gate
(add-action! a or-action-procedure)
(add-action! b or-action-procedure)

;; and-gate
(add-action! a and-action-procedure)
(add-action! b and-action-procedure)

(add-action! d and-action-procedure)
(add-action! e and-action-procedure)

;; inverter
(add-action! c invert-input)

;; in the case of definition in the book, the callback will run immediately
;; when it is defined and set the correct initial input and output

;; Without the initialization, the function box will remain incorrect until some
;; changes happened to a wire

;; Exercise 3.32
;; (define (and-gate a1 a2 output)
;;   (define (and-action-procedure)
;;     (let ((new-value
;;            (logical-and (get-signal a1) (get-signal a2))))
;;       (after-delay and-gate-delay
;;                    (lambda () (set-signal! output new-value)))))
;;     (add-action! a1 and-action-procedure)
;;     (add-action! a2 and-action-procedure)
;;     'ok)
(define i0 (make-wire))
(define i1 (make-wire))
(define output (make-wire))
(define and-gate-delay 2)

(and-gate i0 i1 output)
;; (add-action! i0 and-proc-cbk)
;; (add-action! i1 and-proc-cbk)

(set-signal! i0 0)
(set-signal! i1 1)
(propagate)

(set-signal! i0 1)
(set-signal! i1 0)

;; the callback in the agenda queue will be as below
;; in a FIFO, this will be executed from first to last, which is correct
;; i0 i1     callback
;; 0  1  0
;; 1  1  1   (set-signal! output 1)
;; 1  0  1   (set-signal! output 0)

;; in a LIFO, the callbacks will be executed from last to first, so
;; the final result will be 1 which is incorrect
