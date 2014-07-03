;; Exercise 1.9
(define inc 1+)
(define dec -1+)

;; recursive process
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(if (= 4 5) 5 (inc (+ (dec 4) b)))
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
(9)

;; iterative process
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
(9)

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; ...
;; (A 0 ... (A 1 1))
;; 2^10

(A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 4))
;; (A 1 16)
;; 2^16

(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 4)
;; 2^16

(define (f n) (A 0 n))
;; O(0)
;; f(n) = 2n
;; could be reduced to
(define (f n) (* 2 n))

(define (g n) (A 1 n))
;; O(n)
;; g(n) = 2^n
;; could be reduced to
(define (g n)
  (if (= n 1)
      2
      (* 2 (g (- n 1)))))

(define (h n) (A 2 n))
;; O() ?
;; h(n) = 2^h(n-1)

;; Challenge: write the coin-exchange in iterative process

;; Challenge: write the tower-move in iterative process

;; Exercise 1.11

;; recursive process
(define (func n)
  (if (< n 3)
      n
      (+
       (* 1 (func (- n 1)))
       (* 2 (func (- n 2)))
       (* 3 (func (- n 3))))))

;; iterative process
(define (func n)
  (define (func-iter a b c count)
    (if (= count n)
        (+ a (* 2 b) (* 3 c))
        (func-iter
         (+ a (* 2 b) (* 3 c))
         a
         b
         (+ count 1))))
  (if (< n 3)
      n
      (func-iter 2 1 0 3)))

;; Exercise 1.12
;; Pascal’s triangle
(define (pascal row column)
  (cond ((< row column) 0)
        ((or (= row column) (= column 1)) 1)
        (else
         (+
          (pascal (- row 1) (- column 1))
          (pascal (- row 1) column)))))
