#lang r5rs
(#%require racket/include)
(include "../lib/arithmetic.ss")
(include "../lib/prisoner.scm")

;; Problem 1
(define (extract-entry game assoc-list)
  (cond ((null? assoc-list)
         (error "no game is found for EXTRACT-ENTRY" game))
        ((equal? game (caar assoc-list)) (car assoc-list))
        (else (extract-entry game (cdr assoc-list)))))

;; test cases
(map (lambda (play)
       (extract-entry (apply make-play play) *game-association-list*))
     '(("c" "c") ("c" "d") ("d" "c") ("d" "d")))
(get-point-list '("c" "c"))

;; Problem 2
(define ORIGINAL-STRATs (list NASTY PATSY SPASTIC EYE-FOR-EYE EGALITARIAN))

(for-each
 (lambda (strat)
   (for-each
    (lambda (oppo-strat)
      (display strat)
      (display " v.s ")
      (display oppo-strat)
      (play-loop strat oppo-strat)
      (newline))
    ORIGINAL-STRATs))
 ORIGINAL-STRATs)

;; Some Observations
;; NASTY never loses its game and wins against PASTY and SPASTIC
;; PASTY never wins, even against SPASTIC
;; SPASTIC loses to NASTY but wins PATSY and EGALITARIAN
;; EYE-FOR-EYE never wins but it also doesn't lose a lot
;; EGALITARIAN only loses to SPASTIC badly

;; Problem 3
;; code of EGALITARIAN

;; (define (EGALITARIAN  my-history other-history)
;;   (define (count-instances-of test hist)
;;     (cond ((empty-history? hist) 0)
;; 	  ((string=? (most-recent-play hist) test)
;; 	   (+ (count-instances-of test (rest-of-plays hist)) 1))
;; 	  (else (count-instances-of test (rest-of-plays hist)))))
;;   (let ((ds (count-instances-of "d" other-history))
;; 	(cs (count-instances-of "c" other-history)))
;;     (if (> ds cs) "d" "c")))

(map (lambda (strat)
       (display strat)
       (time (play-loop NASTY strat))) ORIGINAL-STRATs)

;; for 1000 games,
;; cpu time: 90 real time: 90 gc time: 0
;; for 1500 games,
;; cpu time: 173 real time: 173 gc time: 0

;; internally, EGALITARIAN operates `count-instance-of',
;; which is a linear recursion with regard to the length of other-history
;; so the complexity is O(2k) where k is length of history

;; in the main `play-loop', every strategy is applied n times, with
;; history length from 0 to n - 1
;; so the time complexity of strategy with regard to times of game is
;; O(2 * 1) * 1 + O(2 * 2) * 2 + .... + O(2 * n) * n -> O(2 * n^2)

(define (Egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

;; this Egalitarian recurs on majority-loop and is linear
;; the time complexity is O(k), which is half of original EGALITARIAN
;; the orders of growth is O(n^2)

;; the running result is the same as expected
(time (play-loop NASTY EGALITARIAN))
;; cpu time: 163 real time: 164 gc time: 0
(time (play-loop NASTY Egalitarian))
;; cpu time: 84 real time: 84 gc time: 0

;; Problem 4
(define (EYE-FOR-TWO-EYES my-history other-history)
  (cond ((empty-history? my-history) "c")
        ((string=? "c" (most-recent-play other-history)) "c")
        ((string=? "d" (most-recent-play other-history))
         (EYE-FOR-EYE (rest-of-plays my-history) (rest-of-plays other-history)))))

(map (lambda (strat)
       (display strat)
       (play-loop EYE-FOR-TWO-EYES strat))
     (cons EYE-FOR-TWO-EYES ORIGINAL-STRATs))

;; it behaves similar to EYE-FOR-EYE, but loses more to the SPASTIC
;; because SPASTIC is less unpredictable and will mislead EYE-FOR-TWO-EYES

;; Problem 5

(define (make-eye-for-n-eyes n)
  (if (= n 1)
      EYE-FOR-EYE
      (lambda (my-history other-history)
        (cond ((empty-history? my-history) "c")
              ((string=? "c" (most-recent-play other-history)) "c")
              ((string=? "d" (most-recent-play other-history))
               ((make-eye-for-n-eyes (- n 1))
                (rest-of-plays my-history)
                (rest-of-plays other-history)))))))

(map (lambda (strat)
       (play-loop (make-eye-for-n-eyes 100) strat))
     ORIGINAL-STRATs)

;; the more 'eyes' it place on, the more worse it plays against SPASTIC and NASTY
;; the score against SPASTIC converges on 4
;; against NASTY the choice of "c" will make this strategy lose
;; against SPASTIC, the randomness of choices will fool the strategy to choose "c"

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (let ((total (+ freq0 freq1))
          (round (+ 1 (length my-history))))
      (if (< (remainder round total) freq0)
          (strat0 my-history other-history)
          (strat1 my-history other-history)))))

(map (lambda (strat)
       (display strat)
       (play-loop (make-rotating-strategy
                   NASTY
                   PATSY
                   9
                   1) strat))
     ORIGINAL-STRATs)

;; the score is the weighted average of both strategy
;; (strat0-result * freq0 + strat1-result * freq1) / (freq0 + freq1)

;; Problem 7
(define (make-higher-order-spastic strat-list)
  (lambda (my-history other-history)
    (let* ((round (+ 1 (length my-history)))
           (pick (remainder round (- (length strat-list) 1))))
      ((list-ref strat-list pick) my-history other-history))))

(map (lambda (strat)
       (display strat)
       (play-loop (make-higher-order-spastic ORIGINAL-STRATs)
                  strat))
     ORIGINAL-STRATs)

;; the score will be the average of scores by applying strategy individually

;; Problem 8
(define gentleness-factor 0.1)
(define (gentle strat)
  (define (shuffle)
    (< (/ (random 100) 100) gentleness-factor))

  (lambda (my-history other-history)
    (let ((play (strat my-history other-history)))
      (if (string=? play "d")
          (if (shuffle) "c" play)
          play))))

(map (lambda (strat)
       (display strat)
       (play-loop (gentle NASTY) ;; slightly-gentle-Nasty
                  strat))
     ORIGINAL-STRATs)

;; lose more to NASTY, due to gentleness
(map (lambda (strat)
       (display strat)
       (play-loop (gentle EYE-FOR-EYE) ;; slightly-gentle-Eye-for-Eye
                  strat))
     ORIGINAL-STRATs)

;; Problem 9
(define *game-association-list*
 (list (list (list "c" "c" "c") (list 4 4 4))
       (list (list "c" "c" "d") (list 2 2 5))
       (list (list "c" "d" "c") (list 2 5 2))
       (list (list "d" "c" "c") (list 5 2 2))
       (list (list "c" "d" "d") (list 0 3 3))
       (list (list "d" "c" "d") (list 3 0 3))
       (list (list "d" "d" "c") (list 3 3 0))
       (list (list "d" "d" "d") (list 1 1 1))))

(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history
		  (+ 900 (random 201))))

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
                                       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (rest-of-plays history2)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1)
				     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

;; Problem 10

(define (NASTY-3 my-history other-history other-history2)
  "d")

(define (PATSY-3 my-history other-history other-history2)
  "c")

(define (SPASTIC-3 my-history other-history other-history2)
  (if (= (random 2) 0)
      "c"
      "d"))

(play-loop NASTY-3 PATSY-3 SPASTIC-3)

(define (last-move-defect? history)
  (string=? "d" (most-recent-play history)))

(define (tough-Eye-for-Eye my-history other-history other-history2)
  (cond ((empty-history? my-history) "c")
        ((or (last-move-defect? other-history)
              (last-move-defect? other-history2))
         "d")
        (else "c")))

(define (soft-Eye-for-Eye my-history other-history other-history2)
  (cond ((empty-history? my-history) "c")
        ((and (last-move-defect? other-history)
              (last-move-defect? other-history2))
         "d")
        (else "c")))

(play-loop tough-Eye-for-Eye NASTY-3 NASTY-3)
(play-loop soft-Eye-for-Eye NASTY-3 NASTY-3)

(play-loop tough-Eye-for-Eye PATSY-3 NASTY-3)
(play-loop soft-Eye-for-Eye PATSY-3 NASTY-3)

(play-loop tough-Eye-for-Eye SPASTIC-3 NASTY-3)
(play-loop soft-Eye-for-Eye SPASTIC-3 NASTY-3)

(play-loop tough-Eye-for-Eye SPASTIC-3 PATSY-3)
(play-loop soft-Eye-for-Eye SPASTIC-3 PATSY-3)

;; tough is always better than the soft play

;; Problem 11
;; returns a three player strategy
(define (make-combined-strategies strat1 strat2 combine-strat)
  (lambda (my-history other-history1 other-history2)
    (let ((result1 (strat1 my-history other-history1))
          (result2 (strat2 my-history other-history2)))
      (combine-strat result1 result2))))

(define combined-eye-for-eye (make-combined-strategies
  EYE-FOR-EYE EYE-FOR-EYE
  (lambda (r1 r2)
    (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))))

(play-loop combined-eye-for-eye SPASTIC-3 PATSY-3)

;; Problem 12

;; history-summary data structure
(define (make-record c-count d-count)
  (cons c-count d-count))

(define (record-c record)
  (car record))

(define (record-d record)
  (cdr record))

(define (record-total record)
  (+ (record-c record) (record-d record)))

(define (init-record)
  (make-record 0 0))

;; immutable data structure
(define (inc-record record play)
  (if (string=? play "c")
      (make-record (inc (record-c record)) (record-d record))
      (make-record (record-c record) (inc (record-d record)))))

(require srfi/1)
(define (make-history-summary hist0 hist1 hist2)
  ;; accumulate the history lists on a list of records
  (fold (lambda (p0 p1 p2 result)
           (let ((cc-record (car result))
                 (cd-record (cadr result))
                 (dd-record (caddr result)))
             (cond
              ((and (string=? p1 "c") (string=? p2 "c"))
               (list (inc-record cc-record p0) cd-record dd-record))
              ((and (string=? p1 "d") (string=? p2 "d"))
               (list cc-record cd-record (inc-record dd-record p0)))
              (else (list cc-record (inc-record cd-record p0) dd-record)))))
         (list (init-record) (init-record) (init-record))
         hist0
         (rest-of-plays hist1)
         (rest-of-plays hist2)))

(define summary (make-history-summary
                 (list "c" "c" "d" "d" "c" "d" "c" "c")
                 (list "c" "c" "c" "d" "d" "c" "d" "c")
                 (list "c" "c" "d" "d" "d" "c" "c" "c")))

(define summary-cc car)
(define summary-cd cadr)
(define summary-dd caddr)

;; Problem 13
(define (get-probability-of-c summary)
  (map (lambda (record)
         (if (zero? (record-total record))
             '()
             (/ (record-c record)
                (record-total record))))
       summary))

(get-probability-of-c summary)

(define test-summary (make-history-summary
                 (list "c" "c" "c" "c")
                 (list "d" "d" "d" "c")
                 (list "d" "d" "c" "c")))

(get-probability-of-c test-summary)

(define new-summary (make-history-summary
                     (list "c" "c" "c" "d" "c")
                     (list "d" "c" "d" "d" "c")
                     (list "d" "c" "c" "c" "c")))

(get-probability-of-c new-summary)

;; Problem 14
(define (test-entry index trial)
  (cond ((null? index) (null? trial))
        ((null? trial) #f)
        ((equal? (car index) (car trial)) ;; use equal? instead of =
         (test-entry (cdr index) (cdr trial)))
        (else #f)))

;; always corporate
(define (is-he-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt)
                     (cond ((null? elt) 1)
                           ((= elt 1) 1)
                           (else 0)))
                   (get-probability-of-c (make-history-summary hist0
                                                               hist1
                                                               hist2)))))

(define (is-soft-eye-for-eye? hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (is-hard-eye-for-eye? hist0 hist1 hist2)
  (test-entry (list 1 0 0)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (peeker detector)
  (lambda (hist0 hist1 hist2)
    (displayln (get-probability-of-c
                (make-history-summary hist0 hist1 hist2)))
    (detector hist0 hist1 hist2)))

(define (detect-strat detector strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (detector history0 history1 history2)) ;; detect the 1st strategy
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (+ count 1)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history

                  (+ 900 (random 201))))

(detect-strat is-soft-eye-for-eye? soft-Eye-for-Eye SPASTIC-3 SPASTIC-3)
(detect-strat is-soft-eye-for-eye? tough-Eye-for-Eye SPASTIC-3 SPASTIC-3)
(detect-strat is-hard-eye-for-eye? tough-Eye-for-Eye SPASTIC-3 SPASTIC-3)
(detect-strat could-he-be-a-fool? PATSY-3 SPASTIC-3 SPASTIC-3)
(detect-strat is-he-a-fool? NASTY-3 SPASTIC-3 SPASTIC-3)

(define (NASTY-3 my-history other-history other-history2)
  "d")

(define (dont-tolerate-fools hist0 hist1 hist2)
  (let ((round (length hist0)))
    (if (< round 10) "c"
        (if (and (could-he-be-a-fool? hist1 hist0 hist2)
                 (could-he-be-a-fool? hist2 hist0 hist1))
            "d" "c"))))

(play-loop dont-tolerate-fools PATSY-3 PATSY-3)
(play-loop PATSY-3 PATSY-3 PATSY-3)

(detect-strat (peeker could-he-be-a-fool?) PATSY-3 NASTY-3 dont-tolerate-fools)
