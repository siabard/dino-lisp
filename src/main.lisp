(in-package #:dino-lisp)


(defun game-main ()
  (let ((camera-width 640)
	(camera-height 480))
    (init-global)
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
	(sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	  (setf *renderer* renderer)
	  (sdl2-image:init '(:jpg :png))
	  (sdl2-ttf:init)
	  (init-font "ascii"  "assets/ascii.png")
	  (init-font "hangul" "assets/hangul.png")

	  (let* ((current-time (sdl2:get-ticks))
		 (font-10 (sdl2-ttf:open-font (uiop:merge-pathnames* "assets/notokr-regular.ttf" *application-root*) 14))
		 (texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
		 (tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*)))
		 (panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
		 (trigger-table (make-hash-table :test #'equal))
		 (hero (make-entity :controlable (make-controlable :flag T)
				    :renderable (make-renderable  :texture texture
								  :width 16
								  :height 16
								  )
				    :movable (make-movable :x 0
							   :y 0
							   :new-x 0
							   :new-y 0
							   :dx (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil)
							   :dy (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil)
							   :maxspeed 40)
				    :animatable (make-animatable
						 :elapsed-time 0
						 :animation-span 30
						 :current-animation ""
						 :current-frame 0)))
		 (blue-panel (panel/setup panel-texture 3 3))
		 (hello-textbox (make-textbox :font font-10 :textpanel blue-panel))
		 (textbox (make-dialog-window 40 60 :cols 12 :rows 4
						    :panel blue-panel
						    :texts '("HELLO" "안녕하세요")))
		 (up-door-teleport (action/teleport tiled-map 1 1))
		 (down-door-teleport (action/teleport tiled-map 10 8))
		 (keys (make-instance 'key-input))
		 (mouse-state (make-mouse-system)))
	    (setf (gethash "hero" *entities*) hero)
	    (setf (gethash "mainmap" *tiled-maps*) tiled-map)
	    (setf (gethash "char" *textures*) texture)
	    (setf (gethash "pannel" *textures*) panel-texture)
	    (setf (gethash "font10" *fonts*) font-10)
	    (setf *trigger-table* trigger-table)
	    (trigger/add-enter-action trigger-table 10 7 up-door-teleport)
	    (trigger/add-enter-action trigger-table 8 0 down-door-teleport)
	    (init-keys keys)
	    (entity/make-entity-atlas hero)
	    (entity/make-animation-map hero)
	    (entity/add-animation hero "walk-left" (list 0 1 2))
	    (entity/add-animation hero "idle" (list 0))
	    (entity/set-current-animation hero "walk-left")
	    (textbox/set-text hello-textbox renderer "안녕하세요")
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
			      (sdl2:push-event :quit)))
		       (update-mouses mouse-state)
		       (sdl2:render-clear renderer)
		       (entity/update-input hero keys mouse-state)
		       (entity/pre-update-dt hero)
		       (entity/collide-with-tiled-map hero tiled-map)
		       (entity/update-dt hero dt)
		       (panel/update blue-panel dt)
		       (let ((action (trigger/get-enter-action-with-map-and-entity trigger-table tiled-map hero)))
			 (when action (apply action (list hero))))
		       (multiple-value-bind (cam-x cam-y)
			   (when (entity-movable hero)
			     (tiled/clip-xy tiled-map
					    (movable-x (entity-movable hero))
					    (movable-y (entity-movable hero))
					    camera-width
					    camera-height))
			 (tiled/goto tiled-map cam-x cam-y)
			 (tiled/render renderer tiled-map (sdl2:make-rect 0 0 camera-width camera-height))
			 (entity/render hero renderer cam-x cam-y))
		       (panel/render blue-panel renderer 50 50 120 120)
		       (textbox/render hello-textbox renderer 150 150 200 200)
		       (render-dialog-window textbox :renderer renderer)
		       (sdl2:render-present renderer)
		       (clear-keys keys)
		       (setf current-time (sdl2:get-ticks))
		       (sdl2:delay 8)))
	      (:quit ()
		     (tiled/destroy-texture tiled-map)
		     (delete-global)
		     t))))))))



(defun game-main-2 ()
  (let ((camera-width 640)
	(camera-height 480))
    (init-global)
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
	(sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	  (setf *renderer* renderer)
	  (sdl2-image:init '(:jpg :png))
	  (sdl2-ttf:init)
	  (init-font "ascii"  "assets/ascii.png")
	  (init-font "hangul" "assets/hangul.png")

	  (let* ((current-time (sdl2:get-ticks))
		 (font-10 (sdl2-ttf:open-font (uiop:merge-pathnames* "assets/notokr-regular.ttf" *application-root*) 14))
		 (texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
		 (tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*)))
		 (panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
		 (trigger-table (make-hash-table :test #'equal))
		 (hero (make-entity :controlable (make-controlable :flag T)
				    :renderable (make-renderable  :texture texture
								  :width 16
								  :height 16
								  )
				    :movable (make-movable :x 0
							   :y 0
							   :new-x 0
							   :new-y 0
							   :dx (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil)
							   :dy (make-tween :start 0 :end 0 :timespan 0 :current-time 0 :running nil)
							   :maxspeed 40)
				    :animatable (make-animatable
						 :elapsed-time 0
						 :animation-span 30
						 :current-animation ""
						 :current-frame 0)))
		 (blue-panel (panel/setup panel-texture 3 3))
		 (hello-textbox (make-textbox :font font-10 :textpanel blue-panel))
		 (textbox (make-dialog-window 40 60 :cols 12 :rows 4
						    :panel blue-panel
						    :texts '("HELLO" "안녕하세요")))
		 (up-door-teleport (action/teleport tiled-map 1 1))
		 (down-door-teleport (action/teleport tiled-map 10 8))
		 (keys (make-instance 'key-input))
		 (mouse-state (make-mouse-system)))
	    (setf (gethash "hero" *entities*) hero)
	    (setf (gethash "mainmap" *tiled-maps*) tiled-map)
	    (setf (gethash "char" *textures*) texture)
	    (setf (gethash "pannel" *textures*) panel-texture)
	    (setf (gethash "font10" *fonts*) font-10)
	    (setf *trigger-table* trigger-table)
	    (trigger/add-enter-action trigger-table 10 7 up-door-teleport)
	    (trigger/add-enter-action trigger-table 8 0 down-door-teleport)
	    (init-keys keys)
	    (entity/make-entity-atlas hero)
	    (entity/make-animation-map hero)
	    (entity/add-animation hero "walk-left" (list 0 1 2))
	    (entity/add-animation hero "idle" (list 0))
	    (entity/set-current-animation hero "walk-left")
	    (textbox/set-text hello-textbox renderer "안녕하세요")
	    (let* ((explore-state (make-explore-state :stack nil
						      :mapdata tiled-map
						      :triggers trigger-table
						      :hero hero
						      :camera-width camera-width
						      :camera-height camera-height)))
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
				(sdl2:push-event :quit)))
			 (update-mouses mouse-state)
			 (sdl2:render-clear renderer)

			 (update-state explore-state dt
				       :keyboard keys
				       :mouse :mouse-state)
			 (render-state explore-state renderer)
			 (panel/render blue-panel renderer 50 50 120 120)
			 (textbox/render hello-textbox renderer 150 150 200 200)
			 (render-dialog-window textbox :renderer renderer)
			 (sdl2:render-present renderer)
			 (clear-keys keys)
			 (setf current-time (sdl2:get-ticks))
			 (sdl2:delay 8)))
		(:quit ()
		       (tiled/destroy-texture tiled-map)
		       (delete-global)
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


(defun texture-test ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "test")
      (sdl2:with-renderer (renderer win)
	(let ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*))))
	  (format t "Before Delete : ~A (null? : ~A)~%" texture (autowrap:valid-p texture))
	  (sdl2:destroy-texture texture)
	  (format t "After Delete ~A (null? : ~A)~%" texture (autowrap:valid-p texture)))))))


(defun software-render-test()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "software rendering" :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let ((texture (sdl2:create-texture renderer sdl2:+pixelformat-rgba8888+ sdl2-ffi:+sdl-textureaccess-target+ 320 240)))
	  (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
	  (cffi:with-foreign-pointer (pixels (* 320 240 4))
	    (setf (cffi:mem-ref pixels :uint32 0) #xff000090)
	    (setf (cffi:mem-ref pixels :uint32 4) #xff000090)
	    (setf (cffi:mem-ref pixels :uint32 8) #xff000090)
	    (setf (cffi:mem-ref pixels :uint32 12) #xff0000ff)
	    (setf (cffi:mem-ref pixels :uint32 16) #xff0000ff)
	    (setf (cffi:mem-ref pixels :uint32 20) #xff0000ff)
	    (setf (cffi:mem-ref pixels :uint32 24) #xff0000ff)
	    (sdl2:update-texture texture (sdl2:make-rect 0 0 320 240) pixels (* 320 4))
	    (sdl2:with-event-loop (:method :poll)
	      (:idle ()
		     (sdl2:render-clear renderer)
		     (sdl2:render-copy-ex renderer texture
					  :source-rect (sdl2:make-rect 0 0 320 240)
					  :dest-rect (sdl2:make-rect 0 0 800 600)
					  :angle 0
					  :center (sdl2:make-point 0 0)
					  :flip nil)
		     (sdl2:render-present renderer)
		     (sdl2:delay 8))
	      (:quit ()
		     (sdl2:destroy-texture texture)
		     t))
	    ))))))


(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))



;; (let ((vec (cffi:make-shareable-byte-vector 256)))
;;   (cffi:with-pointer-to-vector-data (ptr vec)
;;     (memset ptr 0 (length vec))))
