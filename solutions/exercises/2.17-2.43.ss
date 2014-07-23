#lang r5rs
(#%require racket/include)
(include "../lib/arithmetic.ss")

;; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(last-pair '(23 72 149 34))

;; Exercise 2.18
(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (coin-value n)
    (cond ((= n 5) 50)
          ((= n 4) 25)
          ((= n 3) 10)
          ((= n 2) 5)
          ((= n 1) 1)
          (else #f)))

;; Exercise 2.19
(define (count-change amount coin-values)
  (let ((no-more? null?)
        (except-first-denomination cdr)
        (first-denomination car))
    (define (cc-iter amount coin-values)
      (cond ((= amount 0) 1)
            ((no-more? coin-values) 0)
            ((< amount 0) 0)
            (else
             (+ (cc-iter amount (except-first-denomination coin-values))
                (cc-iter
                 (- amount (first-denomination coin-values))
                 coin-values)))))
    (cc-iter amount coin-values)))

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(count-change 100 uk-coins)

;; order is insignificant
;; every choices in each recursion step forms a complete set of solutions

;; Exercise 2.20
(define (same-parity base . rest)
  (cons base (filter (lambda (i)
                       (if (even? base) (even? i) (odd? i))) rest)))

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(square-list '(1 2 3 4))

(define (square-list items)
  (map (lambda (i) (square i)) items))

(square-list '(1 2 3 4))

;; Exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list '(1 2 3 4))
;; here is how the process is executed
;; (iter '(2 3 4) (cons 1 '()))
;; (iter '(3 4) (cons 4 '(1)))
;; (iter '(4) (cons 9 '(4 1)))
;; (iter '() (cons 16 '(9 4 1)))
;; '(16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; now it cons a list to an atom, doesn't fix the issue

;; Exercise 2.23
(define (for-each p l)
  (if (null? l)
      #t
      (begin
        (p (car l))
        (for-each p (cdr l)))))

;; Exercise 2.24
(list 1 (list 2 (list 3 4)))
;; [1|-]->[2|-]->[3|4]

;; Exercise 2.25
(car (cdaddr '(1 3 (5 7 9))))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;; Exercise 2.26
(define x '(1 2 3))
(define y '(4 5 6))

(append x y) ;; => (1 2 3 4 5 6)
(cons x y) ;; => ((1 2 3) 4 5 6)
(list x y) ;; => ((1 2 3) (4 5 6))

;; Exercise 2.27
(define (deep-reverse l)
  (cond ((not (pair? l)) l)
        ((null? (cdr l)) (deep-reverse (car l)))
        (else
         (list (deep-reverse (cdr l)) (deep-reverse (car l))))))

(deep-reverse '((1 2) (3 4)))

;; Exercise 2.28
(define (fringe tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

(fringe '(1 (2 (3 4) 5) (6 7)))

;; Exercise 2.29
(define (atom? x) (not (pair? x)))
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; A test mobile:
;; http://community.schemewiki.org/?sicp-ex-2.29
;; Level
;; -----
;; 3                   4  |    8
;;              +---------+--------+ 2
;; 2         3  |  9
;;        +-----+----+ 1
;; 1    1 | 2
;;    +---+---+
;;    2       1

(define level-1-mobile (make-mobile (make-branch 2 1)
                                    (make-branch 1 2)))
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile)
                                    (make-branch 9 1)))
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile)
                                    (make-branch 8 2)))

;; a.
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(left-branch level-2-mobile)
(right-branch level-2-mobile)
(branch-length (left-branch level-2-mobile))
(branch-structure (right-branch level-2-mobile))

;; b.
(define is-weight? atom?)
(define (is-mobile? x) (not (atom? x)))

(define (total-weight m)
  (define (branch-weight b)
    (let ((s (branch-structure b)))
      (if (is-weight? s) s (total-weight s))))
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(total-weight level-1-mobile)
(total-weight level-2-mobile)
(total-weight level-3-mobile)

;; c.

(define (torque b)
  (* (branch-length b) (branch-weight b)))

(torque (left-branch level-3-mobile))

(define (balanced? m)
  (define (balanced-branch? b)
    (let ((s (branch-structure b)))
      (if (is-mobile? s)
          (balanced? s)
          #t)))
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (and (= (torque lb)
            (torque rb))
         (balanced-branch? lb)
         (balanced-branch? rb))))

(balanced? level-1-mobile)
(balanced? level-2-mobile)
(balanced? level-3-mobile)
(balanced? level-3-mobile)

(balanced? (make-mobile (make-branch 3 level-2-mobile)
                                    (make-branch 8 2)))

;; d.
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; only need to change the selector
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((atom? tree) (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (subtree)
         (if (atom? subtree)
             (square subtree)
             (square-tree subtree)))
       tree))

(square-tree '(1 (2 (3 4) 5) (6 7)))

;; Exercise 2.31
(define (tree-map p tree)
  (map (lambda (subtree)
         (if (atom? subtree)
             (p subtree)
             (tree-map p subtree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree '(1 (2 (3 4) 5) (6 7)))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      '(()) ;; a list contains an empty list
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; the difference between an empty list and a list contains an empty list
(let ((rest '()))
  (map (lambda (x) (cons 1 x)) rest))

(let ((rest '(())))
  (map (lambda (x) (cons 1 x)) rest))

;; the idea: partition the subsets of [a1, a2, ... an] into the subset contains a1 and
;; that not. Use [a2, a3, ... an] to compute all subsets without a1, and append a1 to
;; every subset to generate all subsets containing a1

(subsets '(1 2 3))

;; Exercise 2.33
(define (accumulate op init sequence)
  (if (null? sequence) init
      (op (car sequence) (accumulate op init (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (current-coeff higher-terms)
                (+ current-coeff (* higher-terms x))
                ) 0 coefficient-sequence))

(horner-eval 2 '(1 3 0 5 0 1))

;; Exercise 2.35
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (count-leaves tree)
  (accumulate (lambda (leave result) (+ 1 result))
              0 (enumerate-tree tree)))

(define (count-leaves tree)
  (accumulate + 0 (map length (map fringe tree))))

(define x '((1 2) 3 4))
(count-leaves x)

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; Exercise 2.37
(define m1 '((1 3 2) (1 0 0) (1 2 2)))
(define m2 '((0 0 2) (7 5 0) (2 1 1)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (car m1) (car m2))
(dot-product (car m1) (cadr m2))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(matrix-*-vector m1 (caddr m2))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (matrix-*-vector cols r)) m)))

(matrix-*-matrix m2 m1)

(define a '((4 -2) (-2 1)))
(define b '((3 6) (-2 -4)))

(matrix-*-matrix b a)
(matrix-*-matrix a b)

;; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 '(1 2 3))
(fold-left / 1 '(1 2 3))

(fold-right list '() '(1 2 3))
(fold-left list '() '(1 2 3))

;; when op follows the transition rule
;; (= (op a b) (op b a)) for any a, b
;; fold-left and fold-right returns the same result

(fold-right (lambda (x y)
              (/ (min x y) (max x y))) 1 '(1 2 3))
(fold-left (lambda (x y)
              (/ (min x y) (max x y))) 1 '(1 2 3))

;; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (elem result)
                (append result (list elem))) '() sequence))

(reverse '(1 2 3 4 5))

(define (reverse sequence)
  (fold-left (lambda (result elem)
               (cons elem result)) '() sequence))

(reverse '(1 2 3 4 5))

;; Exercise 2.40
(define (enumerate-interval l h)
  (if (> l h)
      '()
      (cons l (enumerate-interval (+ l 1) h))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 10)

;; Exercise 2.41
(define (unique-triple n)
  (flatmap (lambda (x)
             (map (lambda (pair)
                    (cons x pair))
                  (unique-pairs (- x 1))))
           (enumerate-interval 1 n)))

(unique-triple 4)
(define (sum-of-triple triple) (accumulate + 0 triple))

(define (triples-equal-to-sum s n)
  (filter (lambda (triple)
            (= s (sum-of-triple triple)))
          (unique-triple n)))

(triples-equal-to-sum 10 10)

;; Exercise 2.42
;; returns a sequence of all the positions coordinates of queens
(define (queens board-size)
  (let ((empty-board '(()))
        ;; return a new list of boards with new moves at kth col
        (possible-boards (lambda (col prev-solutions)
                           (define (new-positions col)
                             (map (lambda (row)
                                    (list row col)) ;; try every row at kth col
                                  (enumerate-interval 1 board-size)))
                           (flatmap (lambda (move)
                                      (map (lambda (rest-board)
                                             (cons move rest-board))
                                           prev-solutions))
                                    (new-positions col))))
        (safe? (lambda (board)
                 (let ((in-row? (lambda (u v)
                                  (= (car u) (car v))))
                       (in-col? (lambda (u v)
                                  (= (cadr u) (cadr v))))
                       (in-dial? (lambda (u v)
                                   (let ((col-diff (- (car u) (car v)))
                                         (row-diff (- (cadr u) (cadr v))))
                                     (= (abs row-diff) (abs col-diff)))))
                       (this-move (car board))
                       (rest-queens (cdr board)))
                   (not (accumulate (lambda (queue-pos conflict-so-far?)
                                  (if conflict-so-far?
                                      #t
                                      (or (in-row? queue-pos this-move)
                                          (in-col? queue-pos this-move)
                                          (in-dial? queue-pos this-move))))
                                #f rest-queens))))))

    ;; find all the possible board for kth col
    (define (queue-cols k)
      (if (= 0 k)
          empty-board
          (filter safe? (possible-boards k (queue-cols (- k 1))))))
    (queue-cols board-size)))

(queens 4)

(define (display-queens solutions)
  (for-each (lambda (board)
              (displayln board)) solutions))

;; now use the textbook's skeleton
(define (queens board-size)
  (let ((empty-board '())
        (adjoin-position
         (lambda (row col queens)
           (cons (list row col) queens)))
        (safe? (lambda (board)
                 (let ((in-row? (lambda (u v)
                                  (= (car u) (car v))))
                       (in-col? (lambda (u v)
                                  (= (cadr u) (cadr v))))
                       (in-dial? (lambda (u v)
                                   (let ((col-diff (- (car u) (car v)))
                                         (row-diff (- (cadr u) (cadr v))))
                                     (= (abs row-diff) (abs col-diff)))))
                       (this-move (car board))
                       (rest-queens (cdr board)))
                   (not (accumulate (lambda (queue-pos conflict-so-far?)
                                      (if conflict-so-far?
                                          #t
                                          (or (in-row? queue-pos this-move)
                                              (in-col? queue-pos this-move)
                                              (in-dial? queue-pos this-move))))
                                    #f rest-queens))))))
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
           safe?
           (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position
                      new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size)))

;; the enumerations generated by a n board-size game is n*n...*n = n^n
;; the safe? check procedure is O(n)
;; so for the original procedure,the time efficiency is
;; calculated as: O(n^n) * O(n) = O(n^n)

(define (effect-time n)
  (let-values ([(result cpu real gc)
                (time-apply queens (list n))])
    (- real gc)))

(map effect-time '(6 7 8 9 10 11 12))
;; => (1 1 5 22 119 710 3958)

;; Exercise 2.43
;; review the flatmap definition, it will apply the proc to every seq
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position
           new-row k rest-of-queens))
        (enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))
;; for the original program, every step will generate one recursive call to the `queens'
;; the procedure could be interpreted as:
;; 1. generate all the possible queens positions for k-1th column
;; 2. enumerate all rows and add new position at kth column
;; it is a linear recursive procedure

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
;; for the Louis's program, every step will generate board-size recursive calls
;; to `queens', the procedure could be interpreted as:
;; 1. generates all possible row positions for kth column
;; 2. enumerates all the possible queens positions for k-1th column
;; queens are evaluated board-size times every step
;; this is a huge cost for the processes it generate
;; it becomes a tree recursive procedure

;; the shape of tree has board-size depth and board-size degree
;; the number of nodes are
;; (/ (- (expt n n) 1) (- n 1))

;; the original procedure is linear, so the calls to queens are n
;; each calls to queens will generate n enumerations, so in total is n^2
;; so the effect is (/ (- (expt n n) 1) (* n n (- n 1)))
