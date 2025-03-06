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

;; global config
;; note: Only effective when starting up, changing them would not affect the already running instance.
(define *window-title* "game")
(define *window-width* 640)
(define *window-height* 480)
(define *window-fullscreen?* #f)
(define *window-resizable?* #f)
(define *update-hz* 30)
(define *clear-color* black)

(define *board-width* 60)
(define *board-speed-up* 3) ; the ability of speeding up the board when pressing direction keys, the higher the faster
(define *board-slow-down* 5) ; the ability of braking the board when pressing direction keys, the higher the quicker you can stop or change direction

(define *ball-radius* 10)
(define *ball-gravity-y* 0.05) ; the commonly thought gravity, vertically downwards
(define *ball-acceleration* 0.01) ; think of this as the inner engine of the ball, speeding it up in determined pace. well, you can set it to 0 or 0.0 if you don't like this feature and even a negative one if you wish for something like friction
(define *ball-max-speed-x* 10) ; the maximum of the ball velocity on the x-axis, horizonally

(define *ball-boost-when-successfully-bounce-by-board-y* 1) ; everytime the player use the board to hold the ball back from touching the lower end, the ball would get an healthy boost  in velocity upwards 

;; repl server
(define repl (spawn-coop-repl-server))

;; some wrapper as walkarounds for run-game nasty behaviour
(define (draw-wrap alpha)
  (draw alpha))
(define (update-wrap dt)
  (update dt))

;;; real main procedures

(define (load)
  (if #f #f))

(define (draw alpha)
  ;; draw a word
  (draw-text "breakout"
	     (vec2 220.0 400.0)
	     #:scale (vec2 3.0 3.0))
  ;; draw ball and board
  (draw-ball *ball*)
  (draw-board *board*))

(define (update dt)
  ;; repl server
  (poll-coop-repl-server repl)
  ;; update global agenda
  (update-agenda dt)
  
  ;; collision detect and change velocity
  ;; not implemented yet!
  ;; test stuff
  (ball&board-collide *ball* *board*)
  (restrict-board *board*)
  (restrict-ball *ball*)
  
  ;; update ball and board
  (update-ball *ball*)
  (update-board *board*))

;;; enter your code here

;; ball generator
(define* (make-ball
	  #:key
	  [radius *ball-radius*]
	  [position (vec2 320.0 130.0)]
	  [velocity (vec2 0.0 0.0)]
	  [gravity (vec2 0.0 *ball-gravity-y*)]
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

;; brick generator
(define* (make-brick
	  #:key
	  position
	  [width 30]
	  [height 20])
  (let ([p position]
	[w width]
	[h height])
    (lambda (sym)
      (case sym
	[(position) p]
	[(width) w]
	[(height) h]
	[else `((position . ,p)
		(width . ,w)
		(height . ,h))]))))

;; board generator
(define* (make-board
	  #:key
	  [position (vec2 (- (/ *window-width* 2) (/ *board-width* 2)) 20.0)]
	  [width *board-width*]
	  [height 5]
	  [velocity (vec2 0.0 0.0)])
  (let ([p position]
	[w width]
	[h height]
	[v velocity])
    (lambda* (#:key get)
      (case get
	[(position) p]
	[(width) w]
	[(height) h]
	[(velocity) v]
	[else `((position . ,p)
		(width . ,w)
		(height . ,h)
		(velocity . ,v))]))))

;;; draw procedures for ball, brick and board respectively

(define (draw-ball ball)
  (draw-canvas
   (make-canvas
    (with-style ((fill-color red))
		(fill (circle (ball #:get 'position) (ball #:get 'radius)))))))

(define (draw-brick brick)
  (draw-canvas
   (make-canvas
    (with-style ((fill-color yellow))
		(fill (rectangle (brick 'position) (brick 'width) (brick 'height)))))))

(define (draw-board board)
  (draw-canvas
   (make-canvas
    (with-style ((fill-color white))
		(fill (rectangle (board #:get 'position) (board #:get 'width) (board #:get 'height)))))))

;;; update procedures for the ball and board respectively
;; note: collisions (and the related velocity changes) are handled seperately

(define (update-ball ball)
  ;; use acceleration and gravity to modity velocity
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
      (vec2-add! p accelerated-and-gravitied-v))))

(define (update-board board)
  ;; listen to keyboard input and set the velocity of the board
  ;; update the board position due to velocity
  (let ((p (board #:get 'position))
	(v (board #:get 'velocity))
	(acc 0.1)) ; this is the acceleration of the board when direction keys are pressed
      ;; assign new-v to board velocity and update position
      (set-vec2-x! v (+ (* acc
			   (+ (if (key-pressed? 'left)
				  (if (negative? (vec2-x v)) (- *board-speed-up*) (- *board-slow-down*))
				  0)
			      (if (key-pressed? 'right)
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
      (when (rect-intersects? ball-rect board-rect)
	(set-vec2! ball-v
		   (+ (vec2-x ball-v) (vec2-x board-v))
		   (abs (vec2-y ball-v)))
	;; boost the ball vertically if it touches the board by the upper surface
	(when (>= (rect-bottom ball-rect) (rect-top board-rect))
	  (set-vec2-y! ball-v (+ *ball-boost-when-successfully-bounce-by-board-y* (vec2-y ball-v))))))))
      

;;; initialize stuff in game

(define *ball* (make-ball))
(define *board* (make-board))

;; run game at last
(run-game #:window-title *window-title*
	  #:window-width *window-width*
	  #:window-height *window-height*
	  #:window-fullscreen? *window-fullscreen?*
	  #:window-resizable? *window-resizable?*
	  #:update-hz *update-hz*
	  #:clear-color *clear-color*
	  #:load load
	  #:update update-wrap
	  #:draw draw-wrap)
