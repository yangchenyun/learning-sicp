
(define (rec-fact n)
  (if (<= n 0)
      1
      (* n (rec-fact (dec n)))))

(define (iter-fact n)
  (define (iter n result)
    (if (<= n 0)
        result
        (iter (- n 1) (* n result))))
  (iter n 1))


(define (rec-fib n)
  (if (< n 2)
      n
      (+ (rec-fib (- n 1))
	 (rec-fib (- n 2)))))

(define (iter-fib n)
  (define (iter count a b)
    (if (= count 0)
	b
	(iter (- count 1) (+ a b) a)))
  (iter n 1 0))


(define let*-test1
  '((define (put-in-standard-position curve)
     (let* ((start-point (curve 0))
            (curve-started-at-origin
             ((translate (- (x-of start-point))
                         (- (y-of start-point)))
              curve))
            (new-end-point (curve-started-at-origin 1))
            (theta (atan (y-of new-end-point) (x-of new-end-point)))
            (curve-ended-at-x-axis
             ((rotate-around-origin (- theta)) curve-started-at-origin))
            (end-point-on-x-axis (x-of (curve-ended-at-x-axis 1))))
       ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis)))
    (put-in-standard-position (compose unit-circle double))))

(define let*-test2
  '(let* ((a (+ 2 3))
          (b (inc a))
          (b ((lambda ()
                (define (y) (let* ((a (inc a))
                                   (b (inc a)))
                              (cons b y)))
                y))))
     (not (let* ((c (cdr (b))))
            (equal? c b)))))
