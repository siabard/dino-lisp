(in-package #:dino-lisp)

;;;; list to rect
(defun list-to-sdl2-rect (lst)
  (let ((x (car lst))
	(y (cadr lst))
	(w (caddr lst))
	(h (cadddr lst)))
    (sdl2:make-rect x y w h)))


;;;; safe delete texture
(defun safe-delete-texture (texture)
  (when (autowrap:valid-p texture)
    (sdl2:destroy-texture texture)))


;;;; safe close font
(defun safe-close-font (font)
  (when (autowrap:valid-p font)
    (sdl2-ttf:close-font font)))

;;; rect logical->physical
(defun rect/logical->physical (x y w h &key (sx 1) (sy 1))
  (values (* x sx) (* y sy) (* w sx) (* h sy)))


;; rect physical->logical
(defun rect/physical->logical (x y w h &key (sx 1) (sy 1))
  (values (floor x sx) (floor y sy) (floor w sx) (floor h sy)))
