(in-package #:dino-lisp)

(defun trigger/add-to-tiled-map (tiled-map x y actions)
  (let ((triggers (tiled-map-triggers tiled-map))
	(tile-index (tiled/tile-index tiled-map x y)))
    (setf (gethash tile-index triggers) actions)))


(defun trigger/get-from-tiled-map (tiled-map x y)
  (let ((triggers (tiled-map-triggers tiled-map))
	(tile-index (tiled/tile-index tiled-map x y)))
    (gethash tile-index triggers)))

(defun trigger/get-from-tiled-map-with-entity (tiled-map entity)
  (let* ((x (entity-x entity))
	 (y (entity-y entity))
	 (coord (tiled/coord-to-tile-position tiled-map x y)))
    (trigger/get-from-tiled-map tiled-map (car coord) (cadr coord))))
