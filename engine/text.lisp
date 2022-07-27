(in-package #:dino-lisp)

;;;; porting from https://github.com/Samuel85/SDL_DrawText.git
;;;; original code is c++

(defun text/render-char (renderer font glyphs char x y width height)
  (let ((target-x x)
	(text-boundary-right  (+ x width))
	(text-boundary-bottom (+ y height)))
    (format t "right : ~A~%" text-boundary-right)
    (loop for idx from 0 to (- (length char) 1)
	  do (let* ((ch (string (aref char idx)))
		    (org-glyph (gethash ch glyphs))
		    (glyph (if (not  org-glyph)
			       (let* ((surface (sdl2-ttf:render-utf8-solid font ch 255 255 255 0))
				      (texture (sdl2:create-texture-from-surface renderer surface)))

				 (sdl2:free-surface surface)
				 (setf (gethash ch glyphs) texture)
				 texture)
			       org-glyph))
		    (glyph-width  (sdl2:texture-width  glyph))
		    (glyph-height (sdl2:texture-height glyph))
		    (w (if (>= text-boundary-right (+ target-x glyph-width))
			   glyph-width
			   (- text-boundary-right target-x)))
		    (h glyph-height))
	       (format t "target-x : ~A  glyph-width:  ~A  w : ~A~%" target-x glyph-width  w)
	       (when (> w 0)
		 (sdl2:render-copy-ex renderer
				      glyph
				      :source-rect (sdl2:make-rect 0 0 w h)
				      :dest-rect (sdl2:make-rect target-x y w h)))
	       (incf target-x glyph-width)))))
