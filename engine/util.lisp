(in-package #:dino-lisp)

;;;; list to rect
(defun list-to-sdl2-rect (lst)
  (let ((x (car lst))
	(y (cadr lst))
	(w (caddr lst))
	(h (cadddr lst)))
    (sdl2:make-rect x y w h)))
