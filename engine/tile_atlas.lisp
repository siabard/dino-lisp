(in-package #:dino-lisp)

;;;; Texture 정보를 토대로 Atlas 정보를 만듦

(defun make-tile-atlas (texture tile-width tile-height)
  (let ((texture-height (sdl2:texture-height texture))
	(texture-width  (sdl2:texture-width texture)))
    (make-tile-atlas-raw texture-width texture-height tile-width tile-height)))
  
(defun make-tile-atlas-raw (width height tile-width tile-height)
  (let ((tile-cols (floor width tile-width))
	(tile-rows (floor height tile-height)))
    (loop for y below tile-rows
	  nconcing (loop for x below tile-cols
			 collect (list-to-sdl2-rect (list (* x tile-width) (* y tile-height) tile-width tile-height))))))
