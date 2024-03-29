;;;; sprite 

(in-package #:dino-lisp)

(defstruct sprite texture source-rect dest-rect)

(defun sprite/set-texture (sprite texture)
  (setf (sprite-texture sprite) texture))

(defun sprite/set-source-rect (sprite source-rect)
  (setf (sprite-source-rect sprite) source-rect))

(defun sprite/set-dest-rect (sprite dest-rect)
  (setf (sprite-dest-rect sprite) dest-rect))

(defun sprite/render (sprite renderer)
  (sdl2:render-copy-ex renderer (sprite-texture sprite)
		       :source-rect (sprite-source-rect sprite)
		       :dest-rect (sprite-dest-rect sprite)))

(defun sprite/render-texture (sprite renderer texture)
  (sdl2:render-copy-ex renderer texture
		       :source-rect (sprite-source-rect sprite)
		       :dest-rect (sprite-dest-rect sprite)))

(defun sprite/destroy-texture (sprite)
  (safe-delete-texture (sprite-texture sprite)))
