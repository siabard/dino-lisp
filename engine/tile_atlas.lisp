(in-package #:dino-lisp)

;;;; Texture 정보를 토대로 Atlas 정보를 만듦

(defun make-tile-atlas (texture tile-width tile-height)
  (let* ((texture-height (sdl2:texture-height texture))
	 (texture-width  (sdl2:texture-width texture))
	 (tile-cols (floor texture-width tile-width))
	 (tile-rows (floor texture-height tile-height)))
    (loop for y below tile-rows
	  nconcing (loop for x below tile-cols
			 collect (list-to-sdl2-rect (list (* x tile-width) (* y tile-height) tile-width tile-height))))))
