#lang sicp

(define (make-set l) l)

(define (memq? item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

;; Exercise 2.53
(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; Exercise 2.54

(define (equal? l1 l2)
  (cond ((and (symbol? l1) (symbol? l2))
         (eq? l1 l2))
        ((and (number? l1) (number? l2))
         (= l1 l2))
        ((and (null? l1) (null? l2)) #t)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;; Exercise 2.55
(car ''abracadabra)
;; the interpreter expands the expression as
(car '(quote abracadabra))
;; and apply `car' onto it produce the symbol quote
'quote

;; Exercise 2.56
(define (deriv exp var)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exponentiation base expo)
    (cond
     ((=number? expo 1) base)
     ((=number? expo 0) 1)
     (else (list '** base expo))))

  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))

  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier s) (cadr s))
  (define (multiplicand s) (caddr s))

  (define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
  (define (base s) (cadr s))
  (define (exponent s) (caddr s))

  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(** x 0) 'x)
(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)
(deriv '(** (+ x 2) 3) 'x)
(deriv '(** (+ (** x 2) 2) 3) 'x)
(deriv '(** x n) 'x)

;; Exercise 2.57
(define (deriv exp var)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s)
    (if (= 2 (length (cdr s)))
        (caddr s)
        (cons '+ (cddr s))))

  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier s) (cadr s))
  (define (multiplicand s)
    (if (= 2 (length (cdr s)))
        (caddr s)
        (cons '* (cddr s))))

  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; Exercise 2.58
;; a.
;; simplified conditions:
;; 1. +, * is binary operation
;; 2. expression are fully parenthesized
(define (deriv exp var)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list a1 '+ a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

  (define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
  (define (addend s) (car s))
  (define (augend s) (caddr s))

  (define (product? x) (and (pair? x) (eq? (cadr x) '*)))
  (define (multiplier s) (car s))
  (define (multiplicand s) (caddr s))

  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; b.
;; 1. unnecessary parentheses are dropped
;; 2. multiplication is done before addition

(define (deriv exp var)
  ;;; utility methods
  (define (prefix item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) '())
          (else
           (let ((rest (prefix item (cdr x))))
             (if rest (cons (car x) rest) #f)))))

  (define (suffix item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) (cdr x))
          (else (suffix item (cdr x)))))

  (define (singleton? list)
    (null? (cdr list)))

  (define (min-op exp)
    (cond ((memq? '+ exp) '+)
          ((memq? '* exp) '*)
          ((memq? '** exp) '**)
          (else (error "unknown operator for expression" exp))))

  ;; domain-related methods
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list a1 '+ a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))
  (define (make-exponentiation base expo)
    (cond
     ((=number? expo 1) base)
     ((=number? expo 0) 1)
     (else (list base '** expo))))

  (define (sum? x)
    (and (pair? x) (eq? (min-op x) '+)))
  (define (addend s)
    (let ((a (prefix '+ s)))
      (if (singleton? a) (car a) a)))
  (define (augend s)
    (let ((a (suffix '+ s)))
      (if (singleton? a) (car a) a)))

  (define (product? x)
    (and (pair? x) (eq? (min-op x) '*)))
  (define (multiplier s)
    (let ((a (prefix '* s)))
      (if (singleton? a) (car a) a)))
  (define (multiplicand s)
    (let ((a (suffix '* s)))
      (if (singleton? a) (car a) a)))

  (define (exponentiation? x)
    (and (pair? x) (eq? (min-op x) '**)))
  (define (base s)
    (let ((a (prefix '** s)))
      (if (singleton? a) (car a) a)))
  (define (exponent s)
    (let ((a (suffix '** s)))
      (if (singleton? a) (car a) a)))

  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3 * (x + x + 2)) 'x)
(deriv '(x + 3 * x + x + 2) 'x)
(deriv '(3 * x + 10 * x + 2 * (3 * x)) 'x)
(deriv '(x * y * (x + 3)) 'x)
(deriv '(x ** y * (x + 3)) 'x)

;; Exercise 2.59
(define (union-set s1 s2)
  (cond
   ((null? s1) s2)
   ((element-of-set? (car s1) s2)
    (union-set (cdr s1) s2))
   (else (cons (car s1)
               (union-set (cdr s1) s2)))))

;; Exercise 2.60
;; Represent set as list with duplicate elements
;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

;; as duplicates are allowed, just cons the element to the set, O(1)
(define (adjoin-set x set)
  (cons x set))

;; perform the direct append O(1)
(define (union-set s1 s2)
  (append s1 s2))

;; perform O(n^2)
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

;; in application where adjoin-set and union-set is mostly used,
;; this representation is more efficient

(let ((s (make-set '(1 2 3 4))))
  (map (lambda (x)
         (element-of-set? x (adjoin-set x s)))
       '(2 5 a)))

(let ((s (make-set '(1 2 3 4)))
      (t (make-set '(a b c d (1)))))
  (map (lambda (x)
         (equal? (element-of-set? x (union-set t s))
                 (or (element-of-set? x s)
                     (element-of-set? x t))))
       '(2 a '() (1))))

(map (lambda (x)
       (element-of-set? x '()))
     '(2 a '()))

;; Exercise 2.61
;; O(n)
(define (adjoin-set el set)
  (cond
   ((null? set) (list el))
   ((= el (car set)) set)
   ((< el (car set)) (cons el set))
   ((> el (car set)) (cons (car set)
                           (adjoin-set el (cdr set))))))

;; Exercise 2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr s1) (cdr s2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr s1) s2)))
                 ((> x1 x2)
                  (cons x2 (union-set s1 (cdr s2)))))))))

(union-set '(1 2 5 8) '(2 4 8))

;; Exercise 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; a.
;; the produces the same result for every tree
;; the procedure they perform is identical:
;; 1. turn the right branch into list
;; 2. cons the entry to the right list
;; 3. turn the left branch into list and append to the above list

;; ORIGINAL ANSWER, failed to consider the `cons' and `append' complexity
;; b.
;; tree->list-1 will be applied to every node of the tree
;; including the empty subtree for all leaves
;; to convert a balanced n elements tree,
;; for a balanced tree, the depth will be log(n) + 1
;; so the time complexity is O(2^log(n)) and is O(2n)

;; tree->list-2 will be applied to every node of the tree as well
;; to convert a balanced n elements tree,

;; so tree->list-2 is more efficient

(define (make-balanced-tree n)
  (let* ((midpoint (ceiling (/ n 2)))
         (left (if (even? n) (- (- n 1) midpoint) (- n midpoint)))
         (right (- n midpoint)))
    (cond ((= n 0) '())
          ((= n 1) (make-tree midpoint '() '()))
          (else (make-tree midpoint
                           (make-balanced-tree left)
                           (make-balanced-tree right)
                           )))))

(time (begin (tree->list-2 (make-balanced-tree 1000000)) #t))
(time (begin (tree->list-1 (make-balanced-tree 1000000)) #t))

;; CORRECT ANSWER, consider the complexity of cons and append

;; for tree->list-1, there will be 1 `append' and 1 `cons' for each node
;; cons is O(1), append is O(n), so the complexity if O(n^2)

;; for tree->list-2, there will be 1 `cons' for each node,
;; cons is O(1), so the complexity is O(n)


;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; a.
;; partial-tree builds a balanced tree from left to right
;; it first builds the left tree recursively by:
;; 1. calculates the left-tree size with `quotient'
;; 2. passed in the elements to be used.
;; then it fetches the current entry by:
;; 1. `car' on the rest elements
;; later, it builds the right subtree by:
;; 1. calculates the right-tree size, by n - 1 - left-size
;; 2. passed in the remaining elements from above
;; finally, it put the tree part together with `make-tree'
;; and builds a list with the tree and remaining elements

(partial-tree '(1 3 5 7 9 11) 6)
;; left part is built by
(partial-tree '(1 3 5 7 9 11) 2)
;; entry is
(car '(5 7 9 11))
;; right part is built by
(partial-tree (cdr '(5 7 9 11)) 3)

;; b.
;; every step will cut the size of problem in half
;; and each step is a tree recursion
;; so the order of growth is 2^log(n) = n

;; Exercise 2.65
;; using procedures from Exercises 2.62
(define (union-list-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond ((= x1 x2)
                  (cons x1 (union-list-set (cdr s1) (cdr s2))))
                 ((< x1 x2)
                  (cons x1 (union-list-set (cdr s1) s2)))
                 ((> x1 x2)
                  (cons x2 (union-list-set s1 (cdr s2)))))))))

;; tree->list-2, list->tree and union-list-set are all O(n)
;; so the final complexity is O(n)
(define (union-set s1 s2)
  (list->tree
   (union-list-set
    (tree->list-2 s1)
    (tree->list-2 s2))))

(union-set
 (list->tree '(1 3 5 7 9 11))
 (list->tree '(2 4 6 8 10)))

(define (intersection-list-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list-set (cdr s1) (cdr s2))))
              ((< x1 x2)
               (intersection-list-set (cdr s1) s2))
              ((> x1 x2)
               (intersection-list-set s1 (cdr s2)))))))

(define (intersection-set s1 s2)
  (list->tree
   (intersection-list-set
    (tree->list-2 s1)
    (tree->list-2 s2))))

(intersection-set
 (list->tree '(1 3 5 8 9 10))
 (list->tree '(2 3 6 8 10)))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))))

;; for Exercises 2.67 - 2.72
;; Huffman Encoding Trees

;; leave node: symbols that are encoded, weight
;; non-leaf node: a set containing all the symbols in the subtree, sum of weight
;; the path from root to the symbol is the binary encoding representation

;; the data structure
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols-tree tree) (caddr tree))
(define (weight-tree tree) (cadddr tree))

;; generic procedures
(define (leaf? node) (eq? (car node) 'leaf))

(define (symbols node)
  (if (leaf? node)
      (list (symbol-leaf node))
      (symbols-tree node)))

(define (weight node)
  (if (leaf? node)
      (weight-leaf node)
      (weight-tree node)))

(define book-sample-tree
  (make-code-tree ;; 17 {A B C D E F G H}
   (make-leaf 'A 8)
   (make-code-tree ;; 9 {B C D E F G H}
    (make-code-tree ;; 5 {B C D}
     (make-leaf 'B 3)
     (make-code-tree
      (make-leaf 'C 1)
      (make-leaf 'D 1)))
    (make-code-tree ;; 4 {E F G H}
     (make-code-tree ;; 2 {E F}
      (make-leaf 'E 1)
      (make-leaf 'F 1))
     (make-code-tree ;; 2 {G H}
      (make-leaf 'G 1)
      (make-leaf 'H 1))))))

;; decoding procedure
;; the idea is to treat the encoding string as a stack and
;; transform the encoding symbol list "in place"

(define (decode bits tree)
  (define (choose-branch bit branch) ;; to make the proc more robust
    (cond ((= bit 1)
           (right-branch branch))
          ((= bit 0)
           (left-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))

  (define (decode-char bits subtree)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) subtree)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-char (cdr bits) tree)) ;; push on the list in place
              (decode-char
               (cdr bits) next-branch)))))
  (decode-char bits tree))

(define book-sample-code '(1 0 0 0 1 0 1 0))
(decode book-sample-code book-sample-tree) ;; => (B A C)

;; Exercise 2.67
(define sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                     (make-leaf 'B 2)
                                     (make-code-tree
                                      (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;; Exercise 2.68
(define (encode message tree)
  (define (encode-symbol char tree)
    (if (null? tree)
        (error "ENCODE: cannot encode the symbol" char)
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond
           ((and (leaf? left)
                 (eq? (symbol-leaf left) char))
            '(0))
           ((and (leaf? right)
                 (eq? (symbol-leaf right) char))
            '(1))
           ((memq char (symbols left))
            (cons 0 (encode-symbol char left)))
           ((memq char (symbols right))
            (cons 1 (encode-symbol char right)))
           (else
            (error "ENCODE: cannot encode the symbol" char))))))

  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define sample-string '(A D A B B C A))
(encode sample-string sample-tree)
;; should be '(0 1 1 0 0 1 0 1 0 1 1 1 0)

;; Exercise 2.69
;; supporting data structure - weighted set
(define (adjoin-set el set)
  (cond ((null? set) (list el))
        ((<= (weight el) (weight (car set)))
         (cons el set))
        (else
         (cons (car set) (adjoin-set el (cdr set))))))

;; test cases with simple pairs
(let ((weight cdr))
  (map (lambda (x)
         (adjoin-set x '((A 2) (B 4) (C 6) (D 8))))
       '((E 1) (E 5) (E 10))))

(make-leaf 'G 1)
;; return an weight-ordered set
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; return a huffman encoding tree
(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let* ((left (car set))
             (right (cadr set))
             (merged (make-code-tree left right)))
        (successive-merge
         (adjoin-set merged (cddr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-huffman-tree (generate-huffman-tree '((A 2) (B 4) (C 6) (D 8))))
(symbols sample-huffman-tree)
(weight sample-huffman-tree)

;; Exercise 2.70
(define rock-song-alphabet
  '((A 2) (GET 2) (SHA 3) (WAH 1)
    (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define rock-song-huffman-tree
  (generate-huffman-tree rock-song-alphabet))

(define lyric
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(length (encode lyric rock-song-huffman-tree))

;; 84 bits are used to encode
;; if we are using fixed length encode, 3 bit will be need for each char
;; so the total will be 108
(* 3 (length lyric))

;; Exercise 2.71
;; the tree is merged strictly from the left to right
;; so there is (n - 1) merges
;; 1 bit for the most frequent
;; (n-1) bit for the least frequent

;; Exercise 2.72
;; for the special cases, every tree forms from a leaf and a subtree
;; the symbols contains for a tree at level k are (n - k)
;; the `encode' procedure in linear recursive.
;; At each step, it apply two procedures:
;; 1. comparison of leaf symbols
;; 2. search char in subtree's symbols set

;; for procedure 1, the order of growth is O(1). Because of `left-branch'
;; and `symbol-leaf' is O(1)
;; However, if the implementation chooses to search first, then this will
;; be O(N)

;; for procedure 2, the order of growth is O(N). Because `right-branch' and
;; `symbols-tree' is constant and `memq' is O(N).

;; For the most frequent symbols, one step is required, the order of growth is O(1)
;; For the least frequent symbols, (n - 1) steps are required,
;; the order of growth is O(N^2) 1 + 2 + ... + (n - 1) = (n + 1) * (n - 1) / 2
