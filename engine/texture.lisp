(in-package #:dino-lisp)

(defun load-texture (renderer path)
  (let* ((surface (sdl2-image:load-image path))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (progn
      (sdl2:free-surface surface)
      texture)))
