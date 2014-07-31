(#%require racket/pretty)

;; methods only useful for mit-scheme compiler, declare to avoid naming error
(define void-proc (lambda (x) (void)))
(define declare void)
(define-syntax integrate-external
  (syntax-rules ()
    ((_ a ...)
     (printf ""))))
(define-syntax integrate-operator
  (syntax-rules ()
    ((_ a ...)
     (printf ""))))
(define generate-uninterned-symbol void)
(define usual-integrations void)
(define load-option void)

;; alias hash-table behaviors as racket hash-table
(define make-symbol-hash-table make-hash)
(define hash-table/clean! hash-clear!)
(define hash-table/clear! hash-clear!)
(define hash-table/key-list hash-keys)
(define hash-table/get hash-ref)
(define hash-table/put! hash-set!)

;; alias for old mit-scheme procedures
(define pp pretty-print)
(define 1+ add1)
(define -1+ sub1)
(define (except-last-pair l) (drop-right l 1))

(define user-initial-environment
  (current-namespace))
(define (for-all? list proc)
  (if (null? list)
      #t
      (let loop ((list list))
        (let ((next (cdr list)))
          (cond
           ((null? next) (proc (car list)))
           ((proc (car list)) (loop next))
           (else #f))))))
