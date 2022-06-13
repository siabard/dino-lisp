(in-package #:dino-lisp)


(defstruct entity texture width height atlas x y)


(defun entity/make-entity-atlas (entity)
  (let* ((texture (entity-texture entity))
	 (tile-width (entity-width entity))
	 (tile-height (entity-height entity))
	 (atlas (make-tile-atlas texture tile-width tile-height)))
    (setf (entity-atlas entity) atlas)))


(defun entity/render (entity renderer)
  (let* ((source-rect (elt (entity-atlas entity) 0))
	 (dest-rect (sdl2:make-rect (entity-x entity) (entity-y entity)
				    (entity-width entity) (entity-height entity))))
    (sdl2:render-copy-ex renderer (entity-texture entity)
			 :source-rect source-rect
			 :dest-rect dest-rect)))

(defun entity/destroy-texture (entity)
  (sdl2:destroy-texture (entity-texture entity)))
