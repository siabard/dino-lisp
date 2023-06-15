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


(defun make-intro-scene (texture logo-map)
  (make-scene "intro"
	      :x 0
	      :y 0
	      :width 480
	      :height 320
	      :zoom 2
	      :entities (build-entity-from-texts texture logo-map)
	      :camera (sdl2:make-rect 0 0 240 160)))

(defun sinbi-main ()
  (sdl2:with-init (:everything)
    (init-game)
    (sdl2:with-window (win :title "Sinbi city" :w 640 :h 480 :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*)))
	       (states nil)
	       (intro-state (make-instance 'game-state
					   :scenes (list (make-intro-scene texture *intro-logo*))))
	       (start-tick (sdl2:get-ticks))
	       (end-tick (sdl2:get-ticks))
	       (dt (- end-tick start-tick))
	       (keys (make-instance 'key-input))
	       (mouse-state (make-mouse-system)))
	  (push intro-state states)
	  (init-keys keys)
	  (sdl2:with-event-loop (:method :poll)
	    (:mousebuttonup (:button button)
			    (cond ((= button 1)
				   (setf (mouse-system-button-l mouse-state) nil))
				  ((= button 3)
				   (setf (mouse-system-button-r mouse-state) nil))))
	    (:mousebuttondown (:button button)
			      (cond ((= button 1)
				     (setf (mouse-system-button-l mouse-state) t))
				    ((= button 3)
				     (setf (mouse-system-button-r mouse-state) t))))
	    (:keyup (:keysym keysym)
		    (keyup-event keys (sdl2:scancode-value keysym)))
	    (:keydown (:keysym keysym)
		      (keydown-event keys (sdl2:scancode-value keysym)))
	    (:idle ()
		   (setf end-tick (sdl2:get-ticks))
		   (setf dt (- end-tick start-tick))
		   (cond ((eql states nil) (sdl2:push-event :quit))
			 (t
			  (progn (sdl2:set-render-draw-color renderer 0 0 0 255)
				 (sdl2:render-clear renderer)
				 (update-state intro-state dt :mouse mouse-state :keyboard keys)
				 (render-state intro-state renderer)
				 (sdl2:render-present renderer)
				 (when (< dt 33)
				   (sdl2:delay (- 33 dt))))))
		   (setf start-tick (sdl2:get-ticks)))
	    (:quit ()
		   (format t "BYE")
		   (sdl2:destroy-texture texture)
		   t)))))))
