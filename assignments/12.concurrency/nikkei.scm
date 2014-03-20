;;;;         England lost her Barings

;;; The collapse of the Baring Brothers establishment 
;;;  provides an abject Leeson for the banking community.
;;;  -- GJS

(define (make-market name initial-price)
  (let ((price initial-price)
	(price-serializer (make-serializer))
	(pending-orders '())
	(orders-serializer (make-serializer)))
    (define (the-market m)
      (cond ((eq? m 'new-price!)	
	     (price-serializer
		     (lambda (update)
		       (set! price (update price)))))	       
	    ((eq? m 'get-price)
	     price)
	    ((eq? m 'new-order!)
	     (orders-serializer
	      (lambda (new-order)
		(set! pending-orders
		      (append pending-orders
			      (list new-order))))))
	    ((eq? m 'execute-an-order)
	     (((orders-serializer
		(lambda ()
		  (if (not (null? pending-orders))
		      (let ((outstanding-order (car pending-orders)))
			(set! pending-orders (cdr pending-orders))
			outstanding-order)
		      (lambda () 'nothing-to-do)))))))
	    ((eq? m 'get-name) name)
	    (else
	     (error "Wrong message" m))))
    the-market))


;;; Using this we make two markets, both starting at the same price:

(define nikkei-fundamental 16680.0)

(define tokyo
  (make-market "Tokyo:Nikkei-225" nikkei-fundamental))

(define singapore
  (make-market "Singapore:Nikkei-225" nikkei-fundamental))

;;; Traders buy and sell contracts at a market using TRANSACT.
;;;  The trader gives the market permission to subtract from the
;;;  trader's monetary balance the cost of the contracts purchased
;;;  and to add to the trader's stash the contracts he purchased.

(define (buy ncontracts market permission)
  (transact ncontracts market permission))

(define (sell ncontracts market permission)
  (transact (- ncontracts) market permission))

(define (transact ncontracts market permission)
  ((market 'new-order!)
   (lambda ()
     (permission (- (* ncontracts (market 'get-price)))
		 ncontracts))))





(define transaction-cost 1.0)

(define (make-arbitrager name balance contracts authorization)
  (let ((trader-serializer (make-serializer)))
    
    (define (change-assets delta-money delta-contracts)
      ((trader-serializer
	(lambda ()
	  (set! balance (+ balance delta-money))
	  (set! contracts (+ contracts delta-contracts))))))

    (define (a<b low-place low-price high-place high-price)
      (if (> (- high-price low-price) transaction-cost)
	  (let ((amount-to-gamble (min authorization balance)))
	    (let ((ncontracts		;round to nearest integer
		   (round (/ amount-to-gamble (- high-price low-price)))))
	      (buy ncontracts low-place change-assets)
	      (sell ncontracts high-place change-assets)))))

    (define (consider-a-trade)
      (let ((nikkei-225-tokyo (tokyo 'get-price))
	    (nikkei-225-singapore (singapore 'get-price)))
	(if (< nikkei-225-tokyo nikkei-225-singapore)
	    (a<b tokyo nikkei-225-tokyo
		 singapore nikkei-225-singapore)
	    (a<b singapore nikkei-225-singapore
		 tokyo nikkei-225-tokyo))))

    (define (me message)
      (cond ((eq? message 'name) name)
	    ((eq? message 'balance) balance)
	    ((eq? message 'contracts) contracts)
	    ((eq? message 'consider-a-trade) (consider-a-trade))
	    (else
	     (error "Unknown message -- ARBITRAGER" message))))
    me))

(define nick-leeson
  (make-arbitrager "Nick Leeson" 1000000000. 0.0 10000.))

;;; The following parameters determine the way the Nikkei average
;;;  drifts over time, and how the two markets differ.  The details of
;;;  this are probably not very much like a real market, and they
;;;  probably do not matter very much for this problem set.

(define nikkei-drift +0.1)
(define nikkei-split +1.0)

(define (nikkei-update)
  (define (average x y) (/ (+ x y) 2.0))
  (gaussian-random-pair
   (lambda (d1 d2)
     ;; d1 and d2 are both distributed as Gaussians 
     ;;  with zero mean and unity standard deviation.
     ;; The fundamental drift
     (set! nikkei-fundamental
	   (+ nikkei-fundamental
	      nikkei-drift
	      (* nikkei-split (+ d1 d2))))
     ;; The actual split -- cannot be modified
     ((tokyo 'new-price!)
      (lambda (old-price)
	(+ (average nikkei-fundamental old-price)
	   (* nikkei-split d1))))
     ((singapore 'new-price!)
      (lambda (old-price)
	   (+ (average nikkei-fundamental old-price)
	      (* nikkei-split d2)))))))

(define (ticker market)
  (newline)
  (display (market 'get-name))
  (display "		")
  (display (market 'get-price)))

(define (audit trader)
  (newline)
  (display (trader 'name))
  (display "	Balance: ")
  (display (trader 'balance))
  (display "	Contracts: ")
  (display (trader 'contracts)))

;;; Delay times are in milliseconds.
(define (do-aperiodically thunk max-delay)
  (define (again)
    (thunk)
    (sleep-current-thread (random max-delay))
    (again))
  again)

(define (do-periodically thunk time)
  (define (again)
    (thunk)
    (sleep-current-thread time)
    (again))
  again)


;;; Students: Please ignore the following hairy expression.  
;;;  It is only here to allow us to reload this file into a
;;;  running simulation.
(if (lexical-unreferenceable? user-initial-environment 'stop-world)
    (define stop-world false))

;;; The following starts a bunch of threads running in parallel
;;;  and makes a procedure to stop all of the parallel threads.

(define (start-world)
    (if stop-world (stop-world))
    (set! stop-world
	  (parallel-execute
	   (do-aperiodically (lambda ()
			       (nikkei-update))
			     3000) 
	   (do-aperiodically (lambda ()
			       (nick-leeson 'consider-a-trade))
			     4000) 
	   (do-aperiodically (lambda ()
			       (audit nick-leeson))
			     20000)
	   (do-aperiodically (lambda ()
			       (tokyo 'execute-an-order))
			     2000)
	   (do-aperiodically (lambda ()
			       (singapore 'execute-an-order))
			     2500)
	   (do-periodically (lambda ()
			      (ticker tokyo))
			    10000)
	   (do-periodically (lambda ()
			      (ticker singapore))
			    10000)
	    ))
    'running)