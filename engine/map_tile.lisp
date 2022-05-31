(in-package #:dino-lisp)

(defstruct map-tile width height layer)

(defun map/set-map-tile-layer (mt width height)
  (setf (map-tile-layer mt) (make-array (* width height))))

(defun map/set-map-tile-layer-xy (mt x y tile)
  (let* ((width (map-tile-width mt))
	 (index (+ x (* y width)))
	 (layer (map-tile-layer mt))
	 (new-layer (setf (aref layer index) tile)))
    (setf (map-tile-layer mt) new-layer))
  mt)

(defun map/render-map-tile (mt renderer texture atlas)
  (let ((width (map-tile-width mt))
	(height (map-tile-height mt)))
    (loop for y below height
	  do (loop for x below width
		   do (let* ((index (+ x (* y width)))
			     (tile (aref (map-tile-layer mt) index))
			     (atl (elt atlas tile))
			     (sprite (make-sprite :texture texture
						  :source-rect atl
						  :dest-rect (sdl2:make-rect (* x width)
									     (* y height)
									     (sdl2:rect-width atl)
									     (sdl2:rect-height atl)))))
			(sprite/render sprite renderer))))))

