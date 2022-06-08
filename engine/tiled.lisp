(in-package #:dino-lisp)

;;;; tiled test-sdl2

(defstruct )

(defstruct tileset-data first-gid atlas)

;; load tilemap
;; return layers and tilesets
(defun load-tiled-map (path-to-map-file)
  "Return tiled map structure. Return NIL when loading is failed"
  (let ((map-path (uiop:merge-pathnames*  path-to-map-file *application-root*)))
    (when (probe-file map-path)
      (cl-tiled:load-map map-path))))


;; make tile atlas from a tileset
(defun make-tile-atlas-from-tileset (tileset)
  (let* ((name (cl-tiled:tileset-name tileset))
	 (tile-width (cl-tiled:tileset-tile-width tileset))
	 (tile-height (cl-tiled:tileset-tile-height tileset))
	 (first-gid (cl-tiled:tileset-first-gid tileset))
	 (tileset-image (cl-tiled:tileset-image tileset))
	 (image-width (cl-tiled:image-width tileset-image))
	 (image-height (cl-tiled:image-height tileset-image))
	 (atlas (make-tile-atlas-raw image-width image-height tile-width tile-height)))
    (list name (make-tileset-data :first-gid first-gid :atlas atlas))))

;; make tile atlas from tilesets
(defun make-tile-atlas-from-tilesets (tilesets)
  (mapcar #'make-tile-atlas-from-tileset tilesets))

;; register to tileatlas info 
