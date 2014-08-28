(require r5rs)

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (memo-proc (lambda () exp)))))

(define (force delayed-object) (delayed-object))

(define stream-car car)
(define (stream-cdr stream)
  (force (cdr stream)))
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low (stream-enumerate-interval (+ low 1) high))))
