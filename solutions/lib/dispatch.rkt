#lang racket
(provide (all-defined-out))

;; for method dispatch table
(define DISPATCH-TABLE (make-hash))
(define (put op type proc)
  (hash-set! DISPATCH-TABLE (cons op type) proc))
(define (get op type)
  (hash-ref DISPATCH-TABLE (cons op type) #f))
(define (dispatch-clear!)
  (hash-clear! DISPATCH-TABLE))

(define COERCION-TABLE (make-hash))
(define (put-coercion from to proc)
  (hash-set! COERCION-TABLE (cons from to) proc))
(define (get-coercion from to)
  (hash-ref COERCION-TABLE (cons from to) #f))
(define (coercion-clear!)
  (hash-clear! COERCION-TABLE))

;; for type-tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; the dispatch methods
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))
