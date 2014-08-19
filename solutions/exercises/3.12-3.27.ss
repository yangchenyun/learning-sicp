#lang r5rs
(require r5rs)

;; Exercise 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x '(a b))
(define y '(c d))
(define z (append x y))
z
(cdr x) ;; (b)

(define w (append! x y))
(cdr x) ;; (b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

;; z is
;; [a | -]-> [b | -]-> [c | -]--
;;  ^                          |
;;  |                          |
;;  ----------------------------

;; call last-pair will result in a infinite loop

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; `loop' will append y to reversed x
;; so `mystery' reverse x

(define v '(a b c d))
(define w (mystery v))
v ;; (a)
w ;; (d c b a)

;; Exercise 3.15
;; set-to-wow! mutates the first element in a pair
;; but as z1 shares the same element in pairs,
;; changes to one will result in changes to another

;; Exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; for pairs without any sharing structure
;; the answer is correct
(define x
  (cons 'a
        (cons 'b
              (cons 'c '()))))
(count-pairs x)

;; sharing one car components
(define w (cons 'a '()))
(define u (cons w (cons w '())))
(count-pairs u)

;; stack pairs one on each other
(define w (cons 'a '()))
(define p (cons w w))
(define v (cons p p))
(count-pairs v)

;; for a cycle made through `make-cycle',
;; `count-pair' will never return

;; Exercise 3.17
(define (count-pairs x)
  (let ((seen '()))
    (define (count-walker x)
      (if (not (pair? x))
          0
          (let* ((seen-list? (memq x seen))
                 (make-count (if seen-list? 0 1)))
            (if seen-list?
                void
                (set! seen (cons x seen)))
            (+ (count-walker (car x))
               (count-walker (cdr x))
               make-count))))
    (count-walker x)))

(count-pairs x)
(count-pairs u)
(count-pairs v)

;; Exercise 3.18
(define (detect-cycle x)
  (let ((seen '()))
    (define (walk-list l)
      (if (null? (cdr l))
          #f
          (let ((next (cadr l)))
            (if (memq next seen)
                #t ;; cycle detected
                (begin
                  (set! seen (cons (car l) seen))
                  (walk-list (cdr l)))))))
    (walk-list x)))

(detect-cycle '(a b c d))
(detect-cycle (make-cycle '(a b c)))

;; this will take space of O(n) for the `seen'

;; Exercise 3.19
;; now use the rabbit and tortoise algorithm
(define (detect-cycle x)
  (define (run-list tor rabbit)
    (if (null? (cddr rabbit)) ;; rabbit will reach the end
        #f
        (let ((next-tor (cdr tor))
              (next-rabbbit (cddr rabbit)))
          (if (eq? next-tor next-rabbbit)
              #t
              (run-list next-tor next-rabbbit)))))
  (run-list x x))

(detect-cycle '(a b c d))
(detect-cycle (make-cycle '(a b c)))

;; Exercise 3.20

;; Exercise 3.21
;; the definition for an empty queue in the data representation is (null? (front-ptr q))
;; the `rear-ptr' is allowed to have non-null values as in the example
(define print-queue car)

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))

    (define (insert! item)
      (let ((new-pair (cons item '())))
        (if (empty?)
            (begin
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr)
            (begin
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr))))
    (define (delete!)
      (if (empty?)
          (error "DELETE!: invalid method on an empty queue")
          (begin
            (set! front-ptr (cdr front-ptr))
            front-ptr)))
    (define (dispatch m)
      (cond
       ((eq? 'empty? m) (empty?))
       ((eq? 'insert! m) insert!)
       ((eq? 'delete! m) (delete!))
       (else (error "unknown methods to a queue"))))
    dispatch))

(define q (make-queue))

((q 'insert!) 'a)
((q 'insert!) 'b)
(q 'delete!)
((q 'insert!) 'c)
((q 'insert!) 'd)
(q 'delete!)

;; Exercise 3.23
;; use a pair to track front-ptr and rear-ptr s usual
;; use double-linked list to keep bi-directional links
(define (make-dequeue)
  (cons '() '()))
(define front-deque car)
(define rear-deque cdr)
(define set-front-deque! set-car!)
(define set-rear-deque! set-cdr!)

;; constructor/selector/mutator for dequeue item
(define (make-dequeue-item item)
  (cons item (cons '() '())))

(define fetch-item car)
(define (prev-ptr dq-item)
  (cadr dq-item))
(define (next-ptr dq-item)
  (cddr dq-item))
(define (set-prev-ptr! dq-item elem)
  (set-car! (cdr dq-item) elem))
(define (set-next-ptr! dq-item elem)
  (set-cdr! (cdr dq-item) elem))

(define (empty-deque? dq)
  (or
   (null? (front-deque dq))
   (null? (rear-deque dq))))

(define (front-insert-deque! dq item)
  (let ((new-item (make-dequeue-item item)))
    (if (empty-deque? dq)
        (begin
          (set-front-deque! dq new-item)
          (set-rear-deque! dq new-item)
          dq)
        (begin
          (set-next-ptr! new-item (front-deque dq))
          (set-prev-ptr! (front-deque dq) new-item)
          (set-front-deque! dq new-item)
          dq))))

(define (rear-insert-deque! dq item)
  (let ((new-item (make-dequeue-item item)))
    (if (empty-deque? dq)
        (begin
          (set-car! dq new-item)
          (set-cdr! dq new-item)
          dq)
        (begin
          (set-prev-ptr! new-item (rear-deque dq))
          (set-next-ptr! (rear-deque dq) new-item)
          (set-rear-deque! dq new-item)
          dq))))


(define (front-delete-deque! dq)
  (if (empty-deque? dq)
      (error "cannot delete on an empty dequeue")
      (begin
        (let ((next (next-ptr (front-deque dq))))
          (set-front-deque! dq next)
          (if (null? next)
              (void)
              (set-prev-ptr! next '()))
          dq))))

(define (rear-delete-deque! dq)
  (if (empty-deque? dq)
      (error "cannot delete on an empty dequeue")
      (begin
        (let ((prev (prev-ptr (rear-deque dq))))
          (set-rear-deque! dq prev)
          (if (null? prev)
              (void)
              (set-next-ptr! prev '()))
          dq))))

(define dq (make-dequeue))

(front-insert-deque! dq 'a)
(front-insert-deque! dq 'b)
(rear-insert-deque! dq 'c)
(rear-insert-deque! dq 'd)

(fetch-item (front-deque dq))
(fetch-item (rear-deque dq))

(front-delete-deque! dq)

(fetch-item (front-deque dq))
(fetch-item (rear-deque dq))

(rear-delete-deque! dq)
(fetch-item (rear-deque dq))
(rear-delete-deque! dq)
(fetch-item (rear-deque dq))
(rear-delete-deque! dq)
(empty-deque? dq)

;; Exercise 3.24
;; this is easy, just use the local predicate

(define (make-table pred)
  (define table (list '*table*))

  (define (assoc key records)
    (cond
     ((null? records) #f)
     ((pred key (caar records)) (car records))
     (else (assoc key (cdr records)))))

  (define (lookup key-1 key-2)
    (let ((subtable (assoc key-1 (cdr table))))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (cdr record)
                #f))
          #f)))
  (define (insert! key-1 key-2 value)
    (let ((subtable (assoc key-1 (cdr table))))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
          (set-cdr! table
                    (cons
                     (cons key-1
                           (list (cons key-2 value)))
                     (cdr table)))))
    'ok)

  (define (dispatch m)
    (cond
     ((eq? m 'lookup) lookup)
     ((eq? m 'insert!) insert!)
     (else (error "Unknown operation: TABLE" m))))

  dispatch)

(define (same-key? a b)
  (<= (abs (- a b)) 0.01))

(define t (make-table same-key?))
((t 'insert!) 10 20 'a)
((t 'lookup) 10.01 19.999)

;; Exercise 3.25
(define (make-table)
  (define local-table (list '*table*))
  (define (make-empty-table name)
    (list name))

  (define (assoc key records)
    (cond
     ((null? records) #f)
     ((equal? key (caar records)) (car records))
     (else (assoc key (cdr records)))))

  (define (find-in-table keys table)
    (if (null? (cdr keys))
        (begin
          (let ((record (assoc (car keys) (cdr table))))
            (if record
                (cdr record)
                #f)))
        (begin
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (find-in-table (cdr keys) subtable)
                #f)))))

  (define (insert-in-table! keys value table)
    (if (null? (cdr keys))
        (begin
          (let ((record (assoc (car keys) (cdr table))))
            (if record
                (set-cdr! record value)
                (set-cdr! table
                          (cons (cons (car keys) value)
                                (cdr table))))))
        (begin
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (insert-in-table! (cdr keys) value subtable)
                (begin
                  (let ((new-subtable (make-empty-table (car keys))))
                    (insert-in-table! (cdr keys) value new-subtable)
                    (set-cdr! table
                              (cons
                               new-subtable
                               (cdr table))))))))))

  (define (lookup keys)
    (find-in-table keys local-table))

  (define (insert! keys value)
    (insert-in-table! keys value local-table)
    'ok)

  (define (dispatch m)
    (cond
     ((eq? m 'lookup) lookup)
     ((eq? m 'insert!) insert!)
     (else (error "Unknown operation: TABLE" m))))
  dispatch)

(define t (make-table))
((t 'insert!) '(letter a) 97)
((t 'insert!) '(letter b) 98)
((t 'insert!) '(math +) 43)
((t 'insert!) '(math -) 45)


((t 'lookup) '(letter a))
((t 'lookup) '(letter b))
((t 'lookup) '(math +))

;; multiple keys
((t 'insert!) '(math complex div) 100)
((t 'lookup) '(math complex div))

;; one key
((t 'insert!) '(a) 1)
((t 'lookup) '(a))

;; Exercise 3.26
;; There are two main changes to the table's backbone.
;; When the table are constructed, is should maintain the ordered property
;; When access tables, `assoc' could utilize the binary tree

;; new data structure
;; table is defined as a binary tree
;; binary tree is used as the backbone of the table
;; the entry is the binary tree is *record*
;; the value of a record should be another table

;; record data structure
(define make-record cons)
(define key car)
(define value cdr)

;; code from 2.3.3, present set as binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; each entry is a record pair

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))))

(define (adjoin-set x set)
  (cond
   ((null? set) (make-tree x '() '()))
   ((= (key (entry set)) (key x)) set)
   ((> (key x) (key (entry set)))
    (make-tree (entry set)
               (left-branch set)
               (adjoin-set x (right-branch set))))
   ((< (key x) (key (entry set)))
    (make-tree (entry set)
               (adjoin-set x (left-branch set))
               (right-branch set)))))

(define (make-table)
  (define local-table '())

  ;; assoc now could take advantage of the binary tree structure
  (define (assoc given-key binary-tree)
    (cond ((null? binary-tree) #f)
          ((= given-key (key (entry binary-tree)))
           (entry binary-tree))
          ((> given-key (key (entry binary-tree)))
           (assoc given-key (right-branch binary-tree)))
          ((< given-key (key (entry binary-tree)))
           (assoc given-key (left-branch binary-tree)))))

  (define (find-in-table keys table)
    (if (null? (cdr keys))
        (begin
          (let ((record (assoc (car keys) table)))
            (if record
                (value record)
                #f)))
        (begin
          (let ((subtable (assoc (car keys) table)))
            (if subtable
                (find-in-table (cdr keys) (value subtable))
                #f)))))

  ;; return the updated table
  (define (insert-in-table! keys value table)
    (if (null? (cdr keys))
        (begin
          (let ((record (assoc (car keys) table)))
            (if record
                (begin
                  (set-cdr! record value)
                  table)
                (adjoin-set (make-record (car keys) value)
                            table))))
        (begin
          (let ((subtable (assoc (car keys) table)))
            (if subtable
                (begin
                  (set-cdr! subtable (insert-in-table! (cdr keys) value (cdr subtable)))
                  table)
                (begin
                  (let ((new-subtable (make-record
                                       (car keys)
                                       (insert-in-table! (cdr keys)
                                                         value
                                                         '()))))
                    (adjoin-set new-subtable table))))))))

  (define (lookup keys)
    (find-in-table keys local-table))

  (define (insert! keys value)
    (set! local-table (insert-in-table! keys value local-table))
    'ok)

  (define (dispatch m)
    (cond
     ((eq? m 'lookup) lookup)
     ((eq? m 'insert!) insert!)
     (else (error "Unknown operation: TABLE" m))))
  dispatch)

(define t (make-table))
((t 'insert!) '(1 2) 97)
((t 'insert!) '(1 10) 98)
((t 'insert!) '(2 8) 108)

((t 'lookup) '(2 8))
((t 'lookup) '(1 10))
((t 'lookup) '(1 2))

;; multiple keys
((t 'insert!) '(1 20 98) 12098)
((t 'lookup) '(1 20 98))

;; one key
((t 'insert!) '(1) 1)
((t 'lookup) '(1))

;; Exercise 3.27
;; `memo-fib' helps to turn an tree recursion into a linear recursion
;; further calls to memo-fib will lookup previous results
;; the table is created by `memorize' in the clojure of a lambda
;; so the storage is persistent

;; (memoize fib) won't have the same effect because in the recursion
;; body of `fib', `fib' is not memorized
