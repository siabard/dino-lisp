(in-package #:dino-lisp)

(defparameter *intro-logo* '("####################"
			     "#                  #"
			     "#  #  #  # # #     #"
			     "# # # #  ### #     #"
			     "#     #  # # #     #"
			     "# #      ### #     #"
			     "# ####             #"
			     "#                  #"
			     "#                  #"
			     "####################"))

(defun build-entity-from-texts (texture texts)
  (let ((entities '())
	(y 0))
    (dolist (text texts)
      (let ((x 0))
	(dolist (c (coerce text 'list))
	  (when (char= #\# c)
	    (let ((entity (make-entity :controlable nil
				       :renderable (make-renderable :texture texture
								    :width 16
								    :height 16)
				       :movable (make-movable :x (* x 16)
							      :y (* y 16)
							      :new-x (* x 16)
							      :new-y (* y 16)
							      :dx (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil)
							      :dy (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil))
				       :animatable (make-animatable
						    :elapsed-time 0
						    :animation-span 30
						    :current-animation ""
						    :current-frame 0))))
	      (entity/make-entity-atlas entity)
	      (entity/make-animation-map entity)
	      (entity/add-animation entity "idle" (list 0))
	      (entity/set-current-animation entity "idle")
	      (push entity entities)))
	  (incf x)))
      (incf y))
    entities))

(defun init-game ()
  (sdl2-image:init '(:jpg :png))
  (init-font "ascii" "assets/ascii.png")
  (init-font "hangul" "assets/hangul.png"))

(defun sinbi-main ()
  (sdl2:with-init (:everything)
    (init-game)
    (sdl2:with-window (win :title "Sinbi city" :w 800 :h 600 :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*)))
	       (states nil)
	       (intro-state (make-instance 'game-state
					   :scenes (list (make-scene "intro"
								      :width 320
								      :height 160
								      :zoom 2
								      :entities (build-entity-from-texts texture *intro-logo*)
								      :camera (sdl2:make-rect 0 0 320 160))))))
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sdl2:render-clear renderer)
		   (render-state intro-state renderer)
		   (sdl2:render-present renderer)
		   (sdl2:delay 8))
	    (:quit ()
		   (sdl2:destroy-texture texture)
		   t)))))))
