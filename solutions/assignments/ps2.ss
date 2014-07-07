#lang r5rs
(#%require rnrs/sorting-6)
(#%require rnrs/lists-6)
(#%require r6rs/private/ports)
(#%require (only racket/base random))

;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]
(define (displayln list)
  (display list)
  (newline))

(define read-line
  (lambda () (get-line (current-input-port))))

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay

;; override in tutorial 1
;; (define (deal) (+ 1 (random 10)))

;; (define (make-new-hand first-card)
;;   (make-hand first-card first-card))

;; (define (make-hand up-card total)
;;   (cons up-card total))

;; (define (hand-up-card hand)
;;   (car hand))

;; (define (hand-total hand)
;;   (cdr hand))

;; (define (hand-add-card hand new-card)
;;   (make-hand (hand-up-card hand)
;;              (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (display "Opponent up card ")
  (displayln opponent-up-card)
  (display "Your Total: ")
  (displayln (hand-total your-hand))
  (display "Hit? ")
  (user-says-y?))

(define (user-says-y?)
  (equal? (read-line) "y"))

(define (display-hands player-hand house-hand)
  (displayln "----- Final Result -----")
  (display "Player Hands: ")
  (displayln (hand-total player-hand))
  (display "House Hands Hands: ")
  (displayln (hand-total house-hand)))

(define (display-game-result game)
  (cond (game (displayln "Player Win!"))
        (else (displayln "House Win!"))))

;; Problem 2
(define (stop-at cap)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand) cap)))

;; (display-game-result (twenty-one hit? (stop-at 16)))

;; Problem 3
(define (test-strategy player-strategy house-strategy number-of-games)
  (letrec ((deck (shuffle-deck (create-deck)))
           ;; the local deal method
           (deal (lambda ()
                   (if (null? deck) (set! deck (shuffle-deck (create-deck))))
                   (let ((card (car deck)))
                     (set! deck (cdr deck))
                     (cons card (+ 1 (random 4))))))

           (twenty-one (lambda (player-strategy house-strategy)
                         (let ((house-initial-hand (make-new-hand (deal))))
                           (let ((player-hand
                                  (play-hand player-strategy
                                             (make-new-hand (deal))
                                             (hand-up-card house-initial-hand))))
                             (if (> (hand-total player-hand) 21)
                                 0                                ; ``bust'': player loses
                                 (let ((house-hand
                                        (play-hand house-strategy
                                                   house-initial-hand
                                                   (hand-up-card player-hand))))
                                   ;; (display-hands player-hand house-hand)
                                   (cond ((> (hand-total house-hand) 21)
                                          1)                      ; ``bust'': house loses
                                         ((> (hand-total player-hand)
                                             (hand-total house-hand))
                                          1)                      ; house loses
                                         (else 0))))))))           ; player loses

           (play-game-once (lambda (player-strategy house-strategy number-of-games)
                             (if (null? deck)
                                 1
                                 (if (= number-of-games 0)
                                     0
                                     (+ (twenty-one player-strategy house-strategy)
                                        (play-game-once player-strategy house-strategy (- number-of-games 1))))))))
    (play-game-once player-strategy house-strategy number-of-games)))

;; (display (test-strategy (stop-at 16) (stop-at 15) 100))

;; Problem 4
(define (watch-player strategy)
  (lambda (player-hand house-up-card)
    (let ((decision (strategy player-hand house-up-card)))
      (display "Opponent up card ")
      (displayln house-up-card)
      (display "Your Total: ")
      (displayln (hand-total player-hand))
      (displayln decision)
      decision)))

;; (displayln (test-strategy (watch-player (stop-at 16)) (watch-player (stop-at 15)) 100))

;; Problem 5
(define (louis player-hand opponent-up-card)
  (cond
   ((< (hand-total player-hand) 12) #t)
   ((> (hand-total player-hand) 16) #f)
   ((= (hand-total player-hand) 12) (< (car opponent-up-card) 4))
   ((= (hand-total player-hand) 16) (= (car opponent-up-card) 10))
   (else (> (car opponent-up-card) 6))))

;; (displayln (test-strategy (stop-at 15) louis 10))
;; (displayln (test-strategy (stop-at 16) louis 10))
;; (displayln (test-strategy (stop-at 17) louis 10))

;; Problem 6
(define (both strategy1 strategy2)
  (lambda (play-hand opponent-up-card)
    (and (strategy1 play-hand opponent-up-card)
         (strategy2 play-hand opponent-up-card))))

;; (displayln (test-strategy (both hit? (stop-at 17)) louis 1))

;; Tutorial 1 Redefine the card/hand data structure
(define (make-new-hand first-card)
  (make-hand first-card (cons first-card '())))

(define (make-hand up-card card-set)
  (cons up-card card-set))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (cons new-card (hand-card-set hand))))

(define (hand-total hand)
  (card-set-total (hand-card-set hand)))

(define (card-set-total card-set)
  (if (null? card-set)
      0
      (+ (card-value (car card-set)) (card-set-total (cdr card-set)))))

;; card: (value suite)
(define (card-value card) (car card))
(define (card-suite card) (car (cdr card)))

;; hand: (up-card card card ...)
(define (hand-up-card hand) (car hand))
(define (hand-card-set hand) (cdr hand))

;; now a card should contain both value and suite
(define (deal) (cons (+ 1 (random 10)) (+ 1 (random 4))))

;; should working properly
;; (displayln (test-strategy (stop-at 15) louis 10))
;; (displayln (test-strategy (stop-at 16) louis 10))
;; (displayln (test-strategy (stop-at 17) louis 10))

;; Tutorial 2
(define (card-from-index i)
  (let ((card (+ 1 (floor (/ i 4)))))
    (if (<= card 10) card 10)))

(define (create-deck)
  (do ((deck '() (cons (card-from-index i) deck))
       (i 54 (- i 1)))
      ((< i 0) deck)))

(define randomp
  (lambda (a b) (< (random 100) 50)))

(define (shuffle-deck deck)
  (list-sort randomp deck))

;; (display (shuffle-deck (create-deck)))

(displayln (test-strategy (stop-at 17) louis 229))
