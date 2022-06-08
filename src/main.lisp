(in-package #:dino-lisp)


(defun game-main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(sdl2-image:init '(:jpg :png))
	(let* ((texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root* )))
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
		   (map/render-map-tile stage-map renderer texture sprite-atlas)
		   (sprite/render sprite renderer)
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
