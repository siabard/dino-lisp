(in-package #:dino-lisp)

(defun load-texture (renderer path)
  (let* ((surface (sdl2-image:load-image path))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (progn
      (sdl2:free-surface surface)
      texture)))

(defun create-gradient-texture (renderer width height start-color end-color)
  (let ((texture (sdl2:create-texture
		  renderer
		  sdl2:+pixelformat-rgba8888+
		  sdl2-ffi:+sdl-textureaccess-target+ width height)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (cffi:with-foreign-pointer (pixels (* width height 4))
      (let* ((s-r (ash start-color -24))
	     (s-g (ash (logand start-color #x00ff0000) -16))
	     (s-b (ash (logand start-color #x0000ff00) -8))
	     (s-a (logand start-color #x000000ff))
	     (e-r (ash end-color -24))
	     (e-g (ash (logand end-color #x00ff0000) -16))
	     (e-b (ash (logand end-color #x0000ff00) -8))
	     (e-a (logand end-color #x000000ff)))
	(loop for y from 0 to (- height 1)
	      do (loop for x from 0 to (- width 1)
		       do (let* ((ratio (/ (* (+ x y) 1.0) (+ width height)))
				 (r (floor (+ (* s-r (- 1 ratio))
					      (* e-r ratio))))
				 (g (floor (+ (* s-g (- 1 ratio))
					      (* e-g ratio))))
				 (b (floor (+ (* s-b (- 1 ratio))
					      (* e-b ratio))))
				 (a (floor (+ (* s-a (- 1 ratio))
					      (* e-a ratio))))
				 (c (+ (ash r 24)
				       (ash g 16)
				       (ash b 8)
				       a)))
			    (setf (cffi:mem-ref pixels :uint32 (+ (* x 4) (* y width 4)))
				  c))))
	(sdl2:update-texture texture (sdl2:make-rect 0 0 width height) pixels (* width 4))))
      texture))
