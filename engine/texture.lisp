(in-package #:dino-lisp)

(defun load-texture (renderer filename)
  (let* ((surface (sdl2-image:load-image (asdf:system-relative-pathname :dino-lisp filename)))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (progn
      (sdl2:free-surface surface)
      texture)))
