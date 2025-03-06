#!/usr/bin/env guile
!#

;;; this is a small single-script templete for writing game in chickadee
;;; feel free to copy it
;;; Usage: just excute this file with guile, then M-x connect-to-guile or M-x geiser-connect with the default host and port (just Ret Ret). After which you can C-x C-f this file and start editing!

;; import some needed modules
(use-modules
 ;; repl server
 (system repl coop-server)
 ;; chickadee
 (chickadee)
 (chickadee audio)
 (chickadee graphics path)
 (chickadee graphics engine)
 (chickadee graphics text)
 (chickadee graphics color)
 (chickadee graphics texture)
 (chickadee math)
 (chickadee math matrix)
 (chickadee math rect)
 (chickadee math vector)
 (chickadee scripting))

;;; global config
;; note: Only effective when starting up, changing them would not affect the already running instance.
(define *window-title* "pong")
(define *window-width* 480)
(define *window-height* 640)
(define *window-fullscreen?* #f)
(define *window-resizable?* #t)
(define *update-hz* 30)
(define *clear-color* black)

(define *window-keyboard-focused?* #f)

(define *board-width* 60)
(define *board-height* 5)
(define *board-distance-from-bottom* 5)
(define *board-speed-up* 3) ; the ability of speeding up the board when pressing direction keys, the higher the faster
(define *board-slow-down* 5) ; the ability of braking the board when pressing direction keys, the higher the quicker you can stop or change direction

(define *ball-radius* 10)
(define *ball-gravity-y* 0.05) ; the commonly thought gravity, vertically downwards
(define *ball-acceleration* 0.001) ; think of this as the inner engine of the ball, speeding it up in determined pace. well, you can set it to 0 or 0.0 if you don't like this feature and even a negative one if you wish for something like friction
(define *ball-max-speed-x* 10) ; the maximum of the ball velocity on the x-axis, horizonally

(define *ball-bounce-on-left-right-wall-velocity-xy-swap-enable?* #t)
(define *ball-bounce-on-left-right-wall-velocity-xy-swap-delay* 1)

(define *ball&board-collide-delay* 2)

;; repl server
(define repl (spawn-coop-repl-server))

;; some wrapper as walkarounds for run-game nasty behaviour
(define (draw-wrap alpha)
  (when *window-keyboard-focused?*
    (draw alpha)))
(define (update-wrap dt)
  ;; repl server
  (poll-coop-repl-server repl)
  
  (when *window-keyboard-focused?*
    (update dt)))

;;; real main procedures

(define (load)
  (if #f #f))

(define (draw alpha)
  ;; draw background
  (draw-canvas
   (make-canvas
    (with-style ((fill-color black))
		(fill (rectangle (vec2 0 0) *window-width* *window-height*)))))
  ;; draw a center line
  (draw-canvas
   (make-canvas
    (with-style ((stroke-color green)
		 (stroke-width 4.0))
		(stroke (line (vec2 0 (/ *window-height* 2))
			      (vec2 *window-width* (/ *window-height* 2)))))))
  ;; draw a word
  (draw-text "PONG"
	     (vec2 (- (/ *window-width* 2) 45)
		   (- (/ *window-height* 2) 10))
	     #:scale (vec2 3.0 3.0))
  ;; draw two sides' scores
  (draw-text (number->string (inexact->exact (score *player-1-score*)))
	     (vec2 0
		   (- (/ *window-height* 2) 40))
	     #:scale (vec2 3.0 3.0))
  (draw-text (number->string (inexact->exact (score *player-2-score*)))
	     (vec2 0
		   (+ (/ *window-height* 2) 5))
	     #:scale (vec2 3.0 3.0))
  ;; draw ball and board
  (draw-ball *ball*)
  (draw-board *board*)
  (draw-board *board-2*))

(define (update dt)
  ;; update global agenda
  (update-agenda dt)

  ;; update score
  (update-score-when-ball-touches-bottom *ball*)

  ;; collision detect and change velocity
  (ball&board-collide *ball* *board*)
  (ball&board-collide *ball* *board-2*)
  (restrict-board *board*)
  (restrict-board *board-2*)
  (restrict-ball *ball*)
  
  ;; update ball and board
  (update-ball *ball*)
  (update-board *board* 'left 'right)
  (update-board *board-2* 'a 'd))

(define (window-keyboard-enter)
  (unless *window-keyboard-focused?*
    (set! *window-keyboard-focused?* #t)))

(define (window-keyboard-leave)
  (when *window-keyboard-focused?*
    (set! *window-keyboard-focused?* #f)))

(define (window-resize width height)
  (unless (= width *window-width*)
    (set! *window-width* width))
  (unless (= height *window-height*)
    (set-vec2-y! (*board-2* #:get 'position) (abs (- *window-height* (*board* #:get 'height) (vec2-y (*board* #:get 'position))))) ; this is a convenient but bad fix when the window is resized. if complexity arose in the future, I would no longer support window resizing to keep the script simple.
    (set! *window-height* height)))

;;; enter your code here

;; ball generator
(define* (make-ball
	  #:key
	  [radius *ball-radius*]
	  [position (vec2 (/ *window-width* 2) 130.0)]
	  [velocity (vec2 0.0 0.0)]
	  [gravity (vec2 0.0 (- *ball-gravity-y*))]
	  [acceleration *ball-acceleration*])
  (let ([r radius]
	[p position]
	[v velocity]
	[g gravity]
	[a acceleration])
    (lambda* (#:key get)
      (case get
	[(radius) r]
	[(position) p]
	[(velocity) v]
	[(gravity) g]
	[(acceleration) a]
	;; outputing everything in else clause is for debug usage and in development stage only.
	;; return '() or *unspecified* if not.
	[else `((radius . ,r)
		(position . ,p)
		(velocity . ,v)
		(gravity . ,g)
		(acceleration . ,a))]))))

;; board generator
(define* (make-board
	  #:key
	  [position (vec2 (- (/ *window-width* 2) (/ *board-width* 2)) *board-distance-from-bottom*)]
	  [width *board-width*]
	  [height *board-height*]
	  [velocity (vec2 0.0 0.0)]
	  [ball&board-collide-enable? #t])
  (let ([p position]
	[w width]
	[h height]
	[v velocity]
	[b ball&board-collide-enable?])
    (lambda* (#:key get ball&board-collide-enable?)
      ;; flip the flag
      (case ball&board-collide-enable?
	[(1) (set! b #t)]
	[(0) (set! b #f)])
      ;; retrieve values
      (case get
	[(position) p]
	[(width) w]
	[(height) h]
	[(velocity) v]
	[(ball&board-collide-enable?) b]
	[else `((position . ,p)
		(width . ,w)
		(height . ,h)
		(velocity . ,v)
		(ball&board-collide-enable? . ,b))]))))

;;; draw procedures for ball, brick and board respectively

(define (draw-ball ball)
  (draw-canvas
   (make-canvas
    (with-style ((fill-color red))
		(fill (circle (ball #:get 'position) (ball #:get 'radius)))))))

(define (draw-board board)
  (draw-canvas
   (make-canvas
    (with-style ((fill-color (if (board #:get 'ball&board-collide-enable?) white red)))
		(fill (rectangle (board #:get 'position) (board #:get 'width) (board #:get 'height)))))))

;;; update procedures for the ball and board respectively
;; note: collisions (and the related velocity changes) are handled seperately

(define (update-ball ball)
  ;; use acceleration and gravity to modity velocity
  ;; update gravity according to the region of the ball
  (let ((p (ball #:get 'position))
	(acc (ball #:get 'acceleration))
	(g (ball #:get 'gravity))
	(v (ball #:get 'velocity)))
    ;; first v should change due to acc
    ;; sencond v should change due to g
    (let* ((delta (vec2* (vec2-normalize v) acc))
	   (accelerated-v (vec2+ v delta))
	   (accelerated-and-gravitied-v (vec2+ accelerated-v g)))
      ;; at last we assign the calculated result back
      (set-vec2! v
		 (vec2-x accelerated-and-gravitied-v)
		 (vec2-y accelerated-and-gravitied-v))
      ;; use velocity to modify position
      (vec2-add! p accelerated-and-gravitied-v))
    ;; update gravity when ball passes the middle horizonal line
    (if (<= (vec2-y p) (/ *window-height* 2))
	(set-vec2-y! g (- (abs (vec2-y g))))
	(set-vec2-y! g (abs (vec2-y g))))))

(define (update-board board key-left key-right)
  ;; listen to keyboard input and set the velocity of the board
  ;; update the board position due to velocity
  (let ((p (board #:get 'position))
	(v (board #:get 'velocity))
	(acc 0.1)) ; this is the acceleration of the board when direction keys are pressed
      ;; assign new-v to board velocity and update position
      (set-vec2-x! v (+ (* acc
			   (+ (if (key-pressed? key-left)
				  (if (negative? (vec2-x v)) (- *board-speed-up*) (- *board-slow-down*))
				  0)
			      (if (key-pressed? key-right)
				  (if (positive? (vec2-x v)) *board-speed-up* *board-slow-down*)
				  0)))
			(vec2-x v)))
      (vec2-add! p v)))

;;; collision detect and velocity change in update procedure
;; intended to be implemented as many procedures, one small task for each.

(define (restrict-board board)
  "prevent board from going outside the screen (mainly left and right wall now) by changing the velocity"
  (let ((p (board #:get 'position))
	(v (board #:get 'velocity)))
    (cond
     [(<= (vec2-x p) 0)
      (set-vec2-x! v (abs (vec2-x v)))]
     [(>= (vec2-x p) (- *window-width* *board-width*))
      (set-vec2-x! v (- (abs (vec2-x v))))])))

(define (restrict-ball ball)
  "prevent ball from going ouside the screen by changing the velocity"
  (let ((p (ball #:get 'position))
	(v (ball #:get 'velocity)))
    (cond
     [(<= (vec2-x p) (ball #:get 'radius))
      (set-vec2-x! v (abs (vec2-x v)))]
     [(>= (vec2-x p) (- *window-width* (ball #:get 'radius)))
      (set-vec2-x! v (- (abs (vec2-x v))))])
    (cond
     [(<= (vec2-y p) (ball #:get 'radius))
      (set-vec2-y! v (abs (vec2-y v)))]
     [(>= (vec2-y p) (- *window-height* (ball #:get 'radius)))
      (set-vec2-y! v (- (abs (vec2-y v))))])

    ;; swap the x and y speed when bouncing against the left and right wall
    (when (and *ball-bounce-on-left-right-wall-velocity-xy-swap-enable?*
	       (or (<= (vec2-x p) (ball #:get 'radius))
		   (>= (vec2-x p) (- *window-width* (ball #:get 'radius)))))
      (set! *ball-bounce-on-left-right-wall-velocity-xy-swap-enable?* #f)
      (let ((v-x (abs (vec2-x v)))
	    (v-y (abs (vec2-y v))))
	(set-vec2! v
		   (* v-y (/ (vec2-x v) v-x))
		   (* v-x (/ (vec2-y v) v-y))))
      (after *ball-bounce-on-left-right-wall-velocity-xy-swap-delay* (set! *ball-bounce-on-left-right-wall-velocity-xy-swap-enable?* #t)))
    
    ;; restrict the x-axis velocity of the ball to prevent it moving horizonally too fast
    (set-vec2-x! v (clamp (- *ball-max-speed-x*) *ball-max-speed-x* (vec2-x v)))))

(define (ball&board-collide ball board)
  "define if ball and board collide and change velocity if so"
  (let ((ball-p (ball #:get 'position))
	(ball-r (ball #:get 'radius))
	(ball-v (ball #:get 'velocity))
	(board-p (board #:get 'position))
	(board-w (board #:get 'width))
	(board-h (board #:get 'height))
	(board-v (board #:get 'velocity)))
    (let ((board-rect (make-rect (vec2-x board-p)
				 (vec2-y board-p)
				 board-w
				 board-h))
	  (ball-rect (make-rect (- (vec2-x ball-p) ball-r)
				(- (vec2-y ball-p) ball-r)
				(* 2 ball-r)
				(* 2 ball-r))))
      (when (and (board #:get 'ball&board-collide-enable?)
		 (rect-intersects? ball-rect board-rect))
	(board #:ball&board-collide-enable? 0)
	(set-vec2! ball-v
		   (+ (vec2-x ball-v) (vec2-x board-v))
		   (if (<= (vec2-y ball-p) (/ *window-height* 2)) ; this is actually fair since when rect intersects, it's impossible for the ball to be in the middle
		       (abs (vec2-y ball-v))
		       (- (abs (vec2-y ball-v)))))
	(after *ball&board-collide-delay*
	       (board #:ball&board-collide-enable? 1))))))

;;; use agenda to note down two sides' score
(define *player-1-score* (make-agenda))
(define *player-2-score* (make-agenda))

(define (score player-agenda)
  (with-agenda player-agenda
	       (agenda-time)))
(define (score+num player-agenda num)
  (with-agenda player-agenda
	       (update-agenda num)))
(define (score+1 player-agenda)
  (score+num player-agenda 1))

(define (update-score-when-ball-touches-bottom ball)
  (let ((p (ball #:get 'position))
	(r (ball #:get 'radius)))
    (cond
     [(<= (vec2-y p) r)
      (score+1 *player-2-score*)]
     [(>= (vec2-y p) (- *window-height* r))
      (score+1 *player-1-score*)])))

;;; initialize stuff in game

(define *ball* (make-ball))
(define *board* (make-board))
(define *board-2* (make-board
		   #:position (vec2 (vec2-x (*board* #:get 'position))
				    (abs (- *window-height* (*board* #:get 'height) (vec2-y (*board* #:get 'position)))))
		   #:width (+ (*board* #:get 'width))
		   #:height (+ (*board* #:get 'height))
		   #:velocity (vec2-copy (*board* #:get 'velocity))))

;; run game at last
(run-game #:window-title *window-title*
	  #:window-width *window-width*
	  #:window-height *window-height*
	  #:window-fullscreen? *window-fullscreen?*
	  #:window-resizable? *window-resizable?*
	  #:window-keyboard-enter window-keyboard-enter
	  #:window-keyboard-leave window-keyboard-leave
	  #:window-resize window-resize
	  #:update-hz *update-hz*
	  #:clear-color *clear-color*
	  #:load load
	  #:update update-wrap
	  #:draw draw-wrap)
