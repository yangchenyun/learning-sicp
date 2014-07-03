;;;	      Structure and Interpretation of Computer Programs
;;;			       Second Edition 
;;;                          Sample Problem Set 
;;;
;;;			    Code file WORLD.SCM


;;;============================================================================
;;; You can extend this file to make more stuff part of your world.
;;;============================================================================

;;;============================================================================
;;; *CAVEAT* To keep your world consistent, whenever you change a procedure or 
;;;          redefine a person/place/etc you should reload this entire file    
;;;          into Scheme. This prevents you from having old-moldy folks running
;;;          around who have not evolved to adhere to your modifications. To   
;;;          make this work out well, you should create little scripts at the  
;;;          end of this file to make the game evolve as you work through it.  
;;;          [See the bottom of this file for an example.]                     
;;;============================================================================


(initialize-clock-list)

;; Here we define the places in our world...
;;------------------------------------------

(define EGG-Atrium   (make-place 'EGG-Atrium))
(define dungeon      (make-place 'dungeon))
(define Building-36  (make-place 'Building-36))
(define computer-lab (make-place 'computer-lab))
(define Tech-Square  (make-place 'Tech-Square))
(define gerry-office  (make-place 'gerry-office))
(define albert-office  (make-place  'albert-office))
(define dormitory    (make-place 'dormitory))

;; The following isolated place is defined in GAME.SCM too but redefined
;; here so you can just "zap" altered definitions there then re-evaluate this
;; file w/o worrying about forgetting to update any places.
;;
;; Consequently, to be consistent, if you find it appropriate to define any new
;; places in GAME.SCM, you should likewise duplicate their definitions here.

(define heaven       (make-place 'heaven))	; The point of no return


;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))

(can-go-both-ways Building-36   'up    'down  computer-lab)
(can-go-both-ways Building-36   'north 'south Tech-Square)
(can-go-both-ways Building-36   'west  'east  EGG-Atrium)
(can-go-both-ways Tech-Square   'up    'down  albert-office)
(can-go-both-ways albert-office 'up    'down  gerry-office)
(can-go-both-ways dormitory     'west  'east  Building-36)

(can-go dungeon      'up    EGG-Atrium)

;; The important critters in our world...
;;---------------------------------------

(define albert   (make&install-person 'albert albert-office 3))
(define gerry    (make&install-person 'gerry  gerry-office  2))

(define grendel  (make&install-troll  'grendel dungeon     4))

(define gerry-card
  (make&install-sd-card 'gerry-card gerry-office '888-12-3456))
(define albert-card
  (make&install-sd-card 'albert-card albert-office '888-98-7654))

;; The beginning of an ever-expanding game script
;;------------------------------------------------

(define (play-game)
  (ask gerry 'take gerry-card)
  (ask gerry 'go 'down)
  (ask gerry 'go 'down)
  )

;; ...now whenever you re-load this file, you can bring things up to
;; date by invoking PLAY-GAME.









