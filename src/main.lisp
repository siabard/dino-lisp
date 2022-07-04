(in-package #:dino-lisp)


(defun game-main ()
  (let ((camera-width 640)
	(camera-height 480))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
	(sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	  (sdl2-image:init '(:jpg :png))
	  (let* ((current-time (sdl2:get-ticks))
		 (texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
		 (tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*)))
		 (hero (make-entity :texture texture
				    :width 16
				    :height 16
				    :x 0
				    :y 0
				    :new-x 0
				    :new-y 0
				    :dx (make-tween :start 0 :end 0 :timespan 0 :current-time 0)
				    :dy (make-tween :start 0 :end 0 :timespan 0 :current-time 0)
				    :maxspeed 40
				    :elapsed-time 0
				    :animation-span 30
				    :current-animation ""
				    :current-frame 0))
		 (keys (make-instance 'key-input))
		 (mouse-state (make-mouse-system)))
	    (init-keys keys)
	    (entity/make-entity-atlas hero)
	    (entity/make-animation-map hero)
	    (entity/add-animation hero "walk-left" (list 0 1 2))
	    (entity/add-animation hero "idle" (list 0))
	    (setf (entity-current-animation hero) "walk-left")
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
		     (let ((dt (- (sdl2:get-ticks) current-time)))
		       (cond ((key-pressed-p keys (sdl2:scancode-key-to-value :scancode-escape))
			      (sdl2:push-event :quit))
			     (t
			      (progn
				(update-mouses mouse-state)
				(sdl2:render-clear renderer)
				(entity/update-input hero keys mouse-state)
				(entity/pre-update-dt hero)
				(entity/collide-with-tiled-map hero tiled-map)
				(entity/update-dt hero dt)
				(multiple-value-bind (cam-x cam-y)
				    (tiled/clip-xy tiled-map (entity-x hero) (entity-y hero) camera-width camera-height)
				  (tiled/goto tiled-map cam-x cam-y)
				  (tiled/render renderer tiled-map (sdl2:make-rect 0 0 camera-width camera-height))
				  (entity/render hero renderer cam-x cam-y))
				(sdl2:render-present renderer)
				(clear-keys keys))))
		       (setf current-time (sdl2:get-ticks))
		       (sdl2:delay 8)))
	      (:quit ()
		     (progn
		       (entity/destroy-texture hero)
		       (tiled/destroy-texture tiled-map)
		       t)))))))))

(defun test-sdl2 ()
  (format t "START~%")
  (sdl2:with-init (:video)
    (format t "INIT~%")
    (sdl2:with-window (win :title "test")
      (sdl2:with-renderer (renderer win)
	(make-tile-atlas (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*)) 16 16)))))

(defun test-sdl2-tile ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "test")

      (sdl2:with-renderer (renderer win)
	(let ((tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*))))
	  tiled-map)))))
