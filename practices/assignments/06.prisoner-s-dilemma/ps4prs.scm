;;; code for PS4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The PLAY-LOOP procedure takes as its arguments two prisoner's 
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds. A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays. The procedure 
;;  returns either the symbol C (for "cooperate") or D ("defect").
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
          (else (let ((result0 (strat0 history0 history1))
                      (result1 (strat1 history1 history0)))
                  (play-loop-iter strat0 strat1 (1+ count)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  limit)))))
  (play-loop-iter strat0 strat1 0 '() '() (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following procedures are used to compute and
;; print out the players' scores at the end of an
;; iterated game.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score: ")
    (display (/ (car scores) number-of-games))
    (newline)
    (display "Player 2 Score: ")
    (display (/ (cadr scores) number-of-games))
    (newline)))


(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0) (list score0 score1))
          (else (let ((game (make-game (most-recent-play history0)
                                       (most-recent-play history1))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (assoc game *game-association-list*)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This list provides the "game matrix". It is used
;; by the scorekeeping procedures above.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *game-association-list*
  '(((c c) (3 3))
    ((c d) (0 5))
    ((d c) (5 0))
    ((d d) (1 1))))
  


;; Some selectors and constructors

(define make-game list)


(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)


;; A sampler of strategies

(define (all-defect my-history other-history)
  'd)

(define (poor-trusting-fool my-history other-history)
  'c)

(define (random-strategy my-history other-history)
  (if (= (random 2) 0) 'c 'd))

(define (go-by-majority my-history other-history)
  (define (count-instances-of symbol hist)
    (cond ((empty-history? hist) 0)
          ((eq? (most-recent-play hist) symbol)
           (1+ (count-instances-of symbol (rest-of-plays hist))))
          (else (count-instances-of symbol (rest-of-plays hist)))))
  (let ((ds (count-instances-of 'd other-history))
        (cs (count-instances-of 'c other-history)))
    (if (> ds cs) 'd 'c)))


(define (tit-for-tat my-history other-history)
  (if (empty-history? my-history)
      'c
      (most-recent-play other-history)))


;;; other possibly useful procedures

(define (get-nth-from-last-play n history)
  (list-ref history n))

(define (get-players-action player-no game)
  (list-ref game player-no))

(define (get-nth-from-last-game n my-history other-history)
  (make-game (get-nth-from-last-play n my-history)
             (get-nth-from-last-play n other-history)))

