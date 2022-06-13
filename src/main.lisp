(in-package #:dino-lisp)


(defun game-main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(sdl2-image:init '(:jpg :png))
	
	(let* ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
	       (tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*)))
	       (hero (make-entity :texture texture
				  :width 16
				  :height 16
				  :x 0
				  :y 0))
	       (keys (make-instance 'key-input))
	       (mouse-state (make-mouse-system)))
	  (init-keys keys)
	  (entity/make-entity-atlas hero)
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
		   (cond ((key-pressed-p keys (sdl2:scancode-key-to-value :scancode-escape))
			  (sdl2:push-event :quit))
			 (t
			  (progn
			    (update-mouses mouse-state)
			    (sdl2:render-clear renderer)
			    (tiled/update-keys tiled-map keys)
			    (tiled/render renderer tiled-map (sdl2:make-rect 0 0 320 240))
			    (entity/render hero renderer)
			    (sdl2:render-present renderer)
			    (clear-keys keys)))))
	    (:quit ()
		   (progn
		     (entity/destroy-texture hero)
		     (tiled/destroy-texture tiled-map)
		     t))))))))


(defun map-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(sdl2-image:init '(:jpg :png))
	(let* ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
	       (tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*)))
	       (sprite (make-sprite :texture texture
				    :source-rect (sdl2:make-rect 0 0 32 32)
				    :dest-rect (sdl2:make-rect 0 0 32 32)))
	       (sprite-atlas (make-tile-atlas texture 16 16))
	       (stage-map (make-map-tile :width 16 :height 16)))
	  (map/set-map-tile-layer stage-map 16 16)
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sprite/set-source-rect sprite (elt sprite-atlas 3))
		   (sprite/set-dest-rect sprite (elt sprite-atlas 0))
		   (sdl2:render-clear renderer)
		   (tiled/render renderer tiled-map (sdl2:make-rect 0 0 320 240))
		   ;(map/render-map-tile stage-map renderer texture sprite-atlas)
		   ;(sprite/render sprite renderer)
		   (sdl2:render-present renderer))
	    (:quit ()
		   (progn
		     (sprite/destroy-texture sprite)
		     t))))))))



(defun test-sdl2 ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "test")
      (sdl2:with-renderer (renderer win)
	(make-tile-atlas (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*)) 16 16)))))

(defun test-sdl2-tile ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "test")
      (sdl2:with-renderer (renderer win)
	(let ((tiled-map (create-tiled-map renderer (uiop:merge-pathnames* "assets/tiled_base64_zlib.tmx" *application-root*))))
	  tiled-map)))))
