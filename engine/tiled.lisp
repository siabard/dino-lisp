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

;; make tile atlas from tilesets and result hashmap
(defun make-tile-atlas-from-tilesets (tilesets)
  (let ((tileset-atlases (mapcar #'make-tile-atlas-from-tileset tilesets))
	(result (make-hash-table :test #'equal)))
    (dolist (atlas tileset-atlases)
      (let ((name (car atlas))
	    (tile-atlas (cadr atlas)))
	(setf (gethash name result) tile-atlas)))
    result))

;; register to tileatlas info 
(defun make-tile-texture-from-tileset (renderer tileset)
  (let* ((name (cl-tiled:tileset-name tileset))
	 (tileset-image (cl-tiled:tileset-image tileset))
	 (image-source (cl-tiled:image-source tileset-image))
	 (texture (load-texture renderer image-source)))
    (list name texture)))

(defun make-tile-texture-from-tilesets (renderer tilesets)
  (let ((textures (mapcar (lambda (tileset) (make-tile-texture-from-tileset renderer tileset)) tilesets))
	(result (make-hash-table :test #'equal)))
    (dolist (texture-info textures)
      (let ((name (car texture-info))
	    (texture (cadr texture-info)))
	(setf (gethash name result) texture)))
    result))
