(define-module (pong)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods))

(define-actor (^ball bcom
		     #:key
		     [radius *ball-radius*]
		     [position (vec2 (/ (window-width *window*) 2) 130.0)]
		     [velocity (vec2 0.0 0.0)]
		     [gravity (vec2 0.0 (- *ball-gravity-y*))]
		     [acceleration *ball-acceleration*])
  (methods
   [(get property)
    (case property
      [(radius) radius]
      [(position) position]
      [(velocity) velocity]
      [(gravity) gravity]
      [(acceleration) acceleration])]
   [(set property value)
    (case property
      [(radius) radius]
      [(position) position]
      [(velocity) velocity]
      [(gravity) gravity]
      [(acceleration) acceleration])]
