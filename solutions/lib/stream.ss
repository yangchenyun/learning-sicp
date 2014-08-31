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

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (append-streams s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (append-streams (stream-cdr s1) s2))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (interleave s1 s2)
  (if (null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream-first s n)
  (if (= n 0)
      'done
      (begin
        (display-line (stream-car s))
        (display-stream-first (stream-cdr s) (- n 1)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low (stream-enumerate-interval (+ low 1) high))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (neg-stream s) (stream-map - s))
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
              s))

(define ones (cons-stream 1 ones))
(define float-ones (cons-stream 1.0 ones))
(define zeros (cons-stream 0 zeros))
(define alts (cons-stream 1 (neg-stream alts)))

(define integers
  (cons-stream 1 (add-streams ones integers)))

;; stream transformer
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ;; Sn-1
        (s1 (stream-ref s 1))   ;; Sn
        (s2 (stream-ref s 2)))  ;; Sn+1
    (cons-stream
     (- s2
        (/ (square (- s2 s1))
           (+ s0 (* -2.0 s1) s2)))
     (euler-transform (stream-cdr s)))))

;; we could create a tableau, a stream of streams

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (integral input initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream input dt)
                  int)))
  int)

(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list)
                   (list->stream (cdr list)))))
