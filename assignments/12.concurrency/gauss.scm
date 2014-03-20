(declare (usual-integrations))

;;; UNIFORM-RANDOM produces an inexact number x,    0 <= x < 1

#|
(define uniform-random
  (let* ((random-max (expt 2 23))
	 (frandom-max (exact->inexact random-max)))
    (lambda ()
      (/ (random random-max)
	 frandom-max))))
|#

(define (uniform-random) (random 1.))

(define (nonzero-uniform-random)
  (let ((x (uniform-random)))
    (if (= x 0.)
	(nonzero-uniform-random)
	x)))

;;; Given uniform random numbers, we can produce pairs of
;;; gaussian-distributed numbers, with zero mean and unit
;;; standard deviation, by the following trick:

(define 2pi (* 4.0 (atan 1.0)))

(define (gaussian-random-pair #!optional continue)
  ;; continue = (lambda (y1 y2) ...)
  (let ((continue (if (default-object? continue) cons continue))
	(x1 (uniform-random))
	(x2 (uniform-random)))
    (let ((r (sqrt (* -2.0 (log x1)))))
      (continue (* r (cos (* 2pi x2)))
		(* r (sin (* 2pi x2)))))))

(define (gaussian-random)
  (gaussian-random-pair (lambda (x y) x)))

(define (gaussian-random-list d)
  (let lp ((j d) (t '()))
    (if (fix:= j 0)
	t
	(gaussian-random-pair
	 (lambda (x1 x2)
	   (if (fix:= j 1)
	       (cons x1 t)
	       (lp (fix:- j 2) (cons x1 (cons x2 t)))))))))


;;; Makes a list of n 2-vectors of gaussian-distributed random numbers  

(define (gaussian-random-pairs n)
  (if (fix:= n 0) 
      '()
      (cons (gaussian-random-pair vector)
	    (gaussian-random-pairs (fix:- n 1)))))


;;; Makes a list of n d-vectors of gaussian-distributed random numbers  

(define (gaussian-random-tuples d n)
  (if (fix:= n 0) 
      '()
      (cons (list->vector (gaussian-random-list d))
	    (gaussian-random-tuples d (fix:- n 1)))))


;;; For adding zero-mean noise with a given standard deviation to a vector.

(define ((add-noise sigma) v)
  (list->vector (map (lambda (signal noise)
		       (+ signal (* sigma noise)))
		     (vector->list v)
		     (gaussian-random-list (vector-length v)))))