(in-package #:dino-lisp)

;;;; porting from https://github.com/Samuel85/SDL_DrawText.git
;;;; original code is c++

(defun text/render-char (renderer font glyphs char x y)
  (let ((target-x x))
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
		    (w (sdl2:texture-width glyph))
		    (h (sdl2:texture-height glyph)))
	       (sdl2:render-copy-ex renderer
				    glyph
				    :source-rect (sdl2:make-rect 0 0 w h)
				    :dest-rect (sdl2:make-rect target-x y w h))
	       (incf target-x  w)))))
