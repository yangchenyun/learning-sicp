;;;;PS8DEFS.SCM

(define (lazy-cons (a lazy) (b lazy))
  (cons (lambda () a) (lambda () b)))
(define lazy-nil '())
(define (lazy-car p) ((car p)))
(define (lazy-cdr p) ((cdr p)))
(define (lazy-null? x) (equal? x lazy-nil))

(define ones (lazy-cons 1 ones))

(define (add-lazy-lists l1 l2)
  (cond ((lazy-null? l1) l2)
        ((lazy-null? l2) l1)
        (else (lazy-cons (+ (lazy-car l1) (lazy-car l2))
                         (add-lazy-lists
                          (lazy-cdr l1)
                          (lazy-cdr l2))))))

(define ints (lazy-cons 0 (add-lazy-lists ones ints)))

(define (lazylist-ref l n)
  (if (<= n 0)
      (lazy-car l)
      (lazylist-ref (lazy-cdr l) (dec n))))

(define (lazy-map f ll)
  (if (lazy-null? ll)
      lazy-nil
      (lazy-cons
       (f (lazy-car ll))
       (lazy-map f (lazy-cdr ll)))))

(define ints
  (lazy-cons
   0
   (lazy-map inc ints)))


(define lazy-list            ;don't apply this till Exercise 6A is completed
  (lambda (l lazy)
    (if (null? l)
        lazy-nil
        (lazy-cons (car l) (lazy-list (cdr l))))))