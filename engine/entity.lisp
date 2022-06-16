(in-package #:dino-lisp)


(defstruct entity
  texture width height atlas x y
  elapsed-time
  animation-span
  current-animation
  current-frame
  animation-map)


(defun entity/make-entity-atlas (entity)
  (let* ((texture (entity-texture entity))
	 (tile-width (entity-width entity))
	 (tile-height (entity-height entity))
	 (atlas (make-tile-atlas texture tile-width tile-height)))
    (setf (entity-atlas entity) atlas)))


(defun entity/make-animation-map (entity)
  (setf (entity-animation-map entity) (make-hash-table :test #'equal)))

(defun entity/add-animation (entity name frames)
  (let ((animation-map (entity-animation-map entity)))
    (setf (gethash name animation-map) frames)))

(defun entity/increase-current-frame (entity)
  (let* ((current-frame (entity-current-frame entity))
	 (animation-map (entity-animation-map entity))
	 (current-animation (entity-current-animation entity))
	 (animation-frames (gethash current-animation animation-map))
	 (next-frame (+ 1 current-frame))
	 (new-current-frame (cond ((>= next-frame (length animation-frames)) 0)
				  (t next-frame))))
    (setf (entity-current-frame entity) new-current-frame)))

(defun entity/increase-elapsed-time-dt (entity dt)
  (let* ((elapsed-time (entity-elapsed-time entity))
	 (next-elapsed-time (+ elapsed-time dt))
	 (animation-span (entity-animation-span entity)))
    (cond ((> next-elapsed-time animation-span)
	   (progn
	     (setf (entity-elapsed-time entity) 0)
	     (entity/increase-current-frame entity)
	     ))
	  (t (setf (entity-elapsed-time entity) next-elapsed-time)))))

(defun entity/update-dt (entity dt)
  (entity/increase-elapsed-time-dt entity dt))

(defun entity/update-input (entity keys mouses)
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
    (decf (entity-x entity)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
    (incf (entity-x entity)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
    (decf (entity-y entity)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
    (incf (entity-y entity))))

(defun entity/render (entity renderer cam-x cam-y)
  (let* ((current-frame (entity-current-frame entity))
	 (current-animation (entity-current-animation entity))
	 (animation-map (entity-animation-map entity))
	 (animation-frames (gethash current-animation animation-map))
	 (pos (elt animation-frames current-frame))
	 (source-rect (elt (entity-atlas entity) pos))
	 (dest-rect (sdl2:make-rect (-  (entity-x entity) cam-x)
				    (-  (entity-y entity) cam-y)
				    (entity-width entity) (entity-height entity))))
    (sdl2:render-copy-ex renderer (entity-texture entity)
			 :source-rect source-rect
			 :dest-rect dest-rect)))

(defun entity/destroy-texture (entity)
  (sdl2:destroy-texture (entity-texture entity)))
