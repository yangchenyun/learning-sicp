
;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* (/ 1 2) a (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2

(define discriminant
  (lambda (a b c)
    (- (* b b) (* 4 a c))))

(define valid_root?
  (lambda (a b c)
    (>= (discriminant a b c) 0)))

(define root1
  (lambda (a b c)
    (if (= a 0)
        (if (= b 0)
            (if (= c 0) #t #f)
            (/ (- c) b))
        (if (valid_root? a b c)
            (/ (- (- b)
                  (sqrt (discriminant a b c)))
               (* 2 a))
            #f))))

(define root2
  (lambda (a b c)
    (if (= a 0)
        (if (= b 0)
            (if (= c 0) #t #f)
            (/ (- c) b))
        (if (valid_root? a b c)
            (/ (+ (- b)
                  (sqrt (discriminant a b c)))
               (* 2 a))
            #f))))

;; complete these procedures and show some test cases
;; case a = 0
;;   case b = 0, c = 0
(root1 0 0 0) ;;#t
;;   case b = 0, c != -0
(root1 0 0 3) ;;#f
;;   case b != 0
(root1 0 3 2) ;;-2/3
;; case: b^2 < 4ac
(root1 4 1 3) ;;#f
;; case: b^2 = 4ac
(= (root1 1 2 1) (root2 1 2 1)) ;;#t
;; normal case
(root1 3 9 4) ;;
(root2 3 9 4) ;;

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root1 (- (/ gravity 2)) vertical-velocity elevation)))
;; use the root1 with - discriminant
;; because the gravity is negative
;;

;; Note that if we want to know when the ball drops to a particular height r
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root1 (- (/ gravity 2)) vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define cal_velocity_y
  (lambda (velocity angle)
    (* (sin (degree2radian angle)) velocity)))

(define cal_velocity_x
  (lambda (velocity angle)
    (* (cos (degree2radian angle)) velocity)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (cal_velocity_x velocity angle)
       (time-to-impact (cal_velocity_y velocity angle)
                       elevation))))

;; let's try this out for some example values.  Note that we are going to
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
(time-to-impact (cal_velocity_y 45 0) 1)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
(time-to-impact (cal_velocity_y 45 90) 1)
;; at an angle of (/ pi 4) radians or 45 degrees
(time-to-impact (cal_velocity_y 45 45) 1)

;; what is the distance traveled in each case?
;; record both in meters and in feet
;; at an angle of 0
(travel-distance-simple 1 45 0)
(meters-to-feet (travel-distance-simple 1 45 0))

;; at an angle of 90
(travel-distance-simple 1 45 90)
(meters-to-feet (travel-distance-simple 1 45 90))

;; at an angle of 45
(travel-distance-simple 1 45 45)
(meters-to-feet (travel-distance-simple 1 45 45))

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 1)

(define find-best-angle
  (lambda (velocity elevation)

    (define best-angle?
      (lambda (angle)
        (if (= angle 0)
            (>= (travel-distance-simple elevation velocity angle)
                (travel-distance-simple
                 elevation
                 velocity
                 (+ angle alpha-increment)))
            (and (>= (travel-distance-simple elevation velocity angle)
                     (travel-distance-simple
                      elevation
                      velocity
                      (+ angle alpha-increment)))
                 (>= (travel-distance-simple elevation velocity angle)
                     (travel-distance-simple
                      elevation
                      velocity
                      (- angle alpha-increment)))))))

    (define try-angle
      (lambda (angle)
        (if (best-angle? angle)
            angle
            (try-angle (+ angle alpha-increment)))))

    (try-angle 0)))


; find best angle
;; try for other velocities
(find-best-angle 45 1)
(find-best-angle 30 1)
(find-best-angle 15 1)
;; try for other heights
(find-best-angle 45 1)
(find-best-angle 45 4)
(find-best-angle 45 10)

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor
;; that can be computed

;; We would like to again compute distance , but taking into account
;; drag.
;; Basically we can rework the equations to get four coupled linear
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))
(define (sqrt x)
  (define (abs x)
    (if (< x 0) (- x) x))
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (<
     (abs (- (/ guess x) (/ (improve guess x) x)))
     0.001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess x))))
  (sqrt-iter 1.0))
;; du = - 1/m speed beta u dt

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (define speed (sqrt (+ (square u0) (square v0))))
    ;; Bug fix here, to calculate when elavation = 0
    (if (< y0 0)
        x0
        (integrate
         (+ x0 (* u0 dt))
         (+ y0 (* v0 dt))
         (- u0 (* (/ 1 m) speed beta u0 dt))
         (- v0 (* (/ 1 m) speed beta v0 dt) (* g dt))
         dt g m beta))))

;; output in the feet unit
(define travel-distance
  (lambda (elavation velocity angle)
    (meters-to-feet
     (integrate
      0
      elavation
      (cal_velocity_x velocity angle)
      (cal_velocity_y velocity angle)
      0.01 gravity mass beta))))

;; RUN SOME TEST CASES
;; speed changes
(travel-distance 1 45 45)
(travel-distance 1 40 45)
(travel-distance 1 35 45)

;; angle changes
(travel-distance 1 45 50)
(travel-distance 1 45 45)
(travel-distance 1 45 40)
(travel-distance 1 45 35)
(travel-distance 1 45 30)
(travel-distance 1 45 25)

;; There is a inmature assumption of this procedure
;; that the function (travel-distance) will only have one peak
;; FIXME
(define find-best-angle
  (lambda (velocity elevation)
    (define best-angle?
      (lambda (angle)
        (if (= angle -90)
            (>= (travel-distance elevation velocity angle)
                (travel-distance
                 elevation
                 velocity
                 (+ angle alpha-increment)))
            (and (>= (travel-distance elevation velocity angle)
                     (travel-distance
                      elevation
                      velocity
                      (+ angle alpha-increment)))
                 (>= (travel-distance elevation velocity angle)
                     (travel-distance
                      elevation
                      velocity
                      (- angle alpha-increment)))))))

    (define try-angle
      (lambda (angle)
        (if (best-angle? angle)
            angle
            (try-angle (+ angle alpha-increment)))))

    (try-angle -90)))

(define find-max-angle
  (lambda (velocity elevation fence)
    (define (try-angle angle)
      (if (< (travel-distance elevation velocity angle) fence)
          angle
          (try-angle (+ angle alpha-increment))))
    (try-angle (find-best-angle velocity elevation))))

(define find-min-angle
  (lambda (velocity elevation fence)
    (define (try-angle angle)
      (if (< (travel-distance elevation velocity angle) fence)
          angle
          (try-angle (- angle alpha-increment))))
    (try-angle (find-best-angle velocity elevation))))

;; The angle ranges is find-min-angle < range < find-max-angle
(find-max-angle 45 1 300)
(find-min-angle 45 1 300)
;; [30...47]

;; what about Denver?
(define density 1.06)
;; [30...47]
(find-max-angle 45 1 300)
(find-min-angle 45 1 300)
;; No significant changes


;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to
;; use, given a velocity, in order to reach a given height (receiver) at a
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft)
;; using 45m/s

;;- Try an angle
;;  - angle > 90?
;;  - yes. return record
;;  - no.
;;    - Update Record
;;
;;  - Update Record
;;    - Calculate the integrated time
;;    - ignore the time if it is 0
;;    - (set! record (min record time))
;;    - Try an Angle + 1

;; distance is provided in meter
(define time-integrate
  (lambda (x0 y0 u0 v0 dt g m beta time distance)
    (define speed (sqrt (+ (square u0) (square v0))))
    (if (< y0 0)
      0
      (if (<= (abs (- x0 distance)) 1)
          time
          (time-integrate
           (+ x0 (* u0 dt))
           (+ y0 (* v0 dt))
           (- u0 (* (/ 1 m) speed beta u0 dt))
           (- v0 (* (/ 1 m) speed beta v0 dt) (* g dt))
           dt g m beta
           (+ time dt)
           distance)))))

(define (cal-time velocity elavation angle distance)
  (time-integrate
   0
   elavation
   (cal_velocity_x velocity angle)
   (cal_velocity_y velocity angle)
   0.01 gravity mass beta 0 distance))

(define shortest-time
  (lambda (velocity elavation distance)
    (define (try-angle angle record)
      (define (update-record record time)
        (set! record
          (min
           record
           (if (zero? time) record time)))
        (try-angle (+ angle 1) record))
      (if (> angle 90)
          record
          (update-record
           record
           (cal-time velocity elavation angle distance))))
      ;; FIXME record should be passed in as an arbitrary number
      (try-angle 30 10000000)))

;; From home plate to second plate
(shortest-time 45 1 36) ;; 1.09
(shortest-time 35 1 36) ;; 1.39
(shortest-time 55 1 36) ;; 0.89

;; 90mph = 40.2336 m /s
(shortest-time 40.2336 1 36) ;; 1.21
;; 3 - 1.21 = 1.79

(shortest-time 45 1 30) ;; 0.87
(shortest-time 45 1 60) ;; 2.07
(shortest-time 45 1 80) ;; 3.09

(shortest-time 35 1 30) ;; 1.12
(shortest-time 35 1 60) ;; 2.65
(shortest-time 35 1 80) ;; never reached
;;  (feet-to-meters (travel-distance 1 35 (find-best-angle 35 1))) 70.84

;; Problem 8
(define (bounce-distance-count elavation velocity angle distance count)
  (if (< count 0)
      distance
      (bounce-distance-count
       0
       (/ velocity 2)
       angle
       (+ distance (travel-distance elavation velocity angle))
       (-1+ count))))

(define (bounce-distance elavation velocity angle distance)
  (if (< velocity 0.1)
      distance
      (bounce-distance
       0
       (/ velocity 2)
       angle
       (+ distance (travel-distance elavation velocity angle)))))

(bounce-distance-count 1 35 40 0 0)
;Value: 233.80223035354834

(bounce-distance-count 1 35 40 0 1)
;Value: 318.38941920775613

(bounce-distance-count 1 35 40 0 2)
;Value: 342.8743281697639

(bounce-distance 1 35 40 0)
;Value: 351.6013244227368

;; Problem 9
(define velocity-integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (define speed (sqrt (+ (square u0) (square v0))))
    (if (< y0 0)
        speed
        (velocity-integrate
         (+ x0 (* u0 dt))
         (+ y0 (* v0 dt))
         (- u0 (* (/ 1 m) speed beta u0 dt))
         (- v0 (* (/ 1 m) speed beta v0 dt) (* g dt))
         dt g m beta))))

(define (cal-velocity velocity elavation angle)
  (velocity-integrate
   0
   elavation
   (cal_velocity_x velocity angle)
   (cal_velocity_y velocity angle)
   0.01 gravity mass beta))

;; FIXME
;; There is a static point for this calculation.
;; You need to Calculate the angle as well

(define (bounce-distance elavation velocity angle distance)
  (if (< velocity 6)
      distance
      (bounce-distance
       0
       (cal-velocity velocity elavation angle)
       angle
       (+ distance (travel-distance elavation velocity angle)))))
