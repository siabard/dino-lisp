(in-package #:dino-lisp)

;;;; tiled test-sdl2

(defstruct tileset-data first-gid atlas texture columns)

;; load tilemap
;; return layers and tilesets
(defun load-tiled-map (path-to-map-file)
  "Return tiled map structure. Return NIL when loading is failed"
  (let ((map-path (uiop:merge-pathnames*  path-to-map-file *application-root*)))
    (when (probe-file map-path)
      (cl-tiled:load-map map-path))))

;; layer를 어떻게 loading 할 것인가??

(defstruct tiled-map layers atlas-texture-table width height tile-width tile-height)

(defun create-tiled-map (renderer path-to-map-file)
  (let ((map-data (load-tiled-map path-to-map-file)))
    (cond (map-data
	   (progn
	     (let* ((layers (cl-tiled:map-layers map-data))
		    (tilesets (cl-tiled:map-tilesets map-data))
		    (width (cl-tiled:map-width map-data))
		    (height (cl-tiled:map-height map-data))
		    (tile-width (cl-tiled:map-tile-width map-data))
		    (tile-height (cl-tiled:map-tile-height map-data))
		    (hashmap-atlas (make-tile-atlas-from-tilesets tilesets))
		    (hashmap-textures (make-tile-texture-from-tilesets renderer tilesets))
		    (atlas-texture-table (merge-tile-atals-texture hashmap-atlas hashmap-textures)))
	       (make-tiled-map :layers layers
			       :width width
			       :height height
			       :tile-width tile-width
			       :tile-height tile-height
			       :atlas-texture-table atlas-texture-table))))
	  (t nil))))

;; make tile atlas from a tileset
(defun make-tile-atlas-from-tileset (tileset)
  (let* ((name (cl-tiled:tileset-name tileset))
	 (tile-width (cl-tiled:tileset-tile-width tileset))
	 (tile-height (cl-tiled:tileset-tile-height tileset))
	 (first-gid (cl-tiled:tileset-first-gid tileset))
	 (columns (cl-tiled:tileset-columns tileset))
	 (tileset-image (cl-tiled:tileset-image tileset))
	 (image-width (cl-tiled:image-width tileset-image))
	 (image-height (cl-tiled:image-height tileset-image))
	 (atlas (make-tile-atlas-raw image-width image-height tile-width tile-height)))
    (list name (make-tileset-data :first-gid first-gid :atlas atlas :texture :nil :columns columns))))

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
    (list name (make-tileset-data :texture texture))))

(defun make-tile-texture-from-tilesets (renderer tilesets)
  (let ((textures (mapcar (lambda (tileset) (make-tile-texture-from-tileset renderer tileset)) tilesets))
	(result (make-hash-table :test #'equal)))
    (dolist (texture-info textures)
      (let ((name (car texture-info))
	    (texture (cadr texture-info)))
	(setf (gethash name result) texture)))
    result))



;; merge texture and atlas
(defun merge-tile-atals-texture (hashmap-atlas hashmap-texture)
  (let ((result (make-hash-table :test #'equal)))
    (loop for k being the hash-keys in hashmap-atlas using (hash-value v)
	  do (setf (gethash k result) v))
    (loop for k being the hash-keys in hashmap-texture using (hash-value v)
	  do (setf (tileset-data-texture (gethash k result)) (tileset-data-texture v)))
    result))


;; translate coordinate to map tile position
(defun tiled/get-tile-xy (tiled-map x y)
  (let ((tile-width (tiled-map-tile-width tiled-map))
	(tile-height (tiled-map-tile-height tiled-map)))
    (list (floor x tile-width) (floor y tile-height))))


;; get hash key from gid
;; layer에는 1부터 시작되므로 이를 줄여야한다.
(defun get-tileset-key-from-gid (tiled-map layer-gid)
  (let* ((first-gids (loop for k being the hash-keys in (tiled-map-atlas-texture-table tiled-map) using (hash-value v)
			   collect (list k (tileset-data-first-gid v))))
	 (gids (mapcar #'cadr first-gids))
	 (filtered-gids (remove-if-not (lambda (elt) (<= elt layer-gid)) gids))
	 (found-first-gid (apply #'max filtered-gids))
	 (filtered-tileset-key (remove-if-not (lambda (lst) (= found-first-gid (cadr lst))) first-gids)))
    (caar filtered-tileset-key)))

;; cell의 row와 column이 출력가능한 영역에 있는지 확인
(defun cell-is-in-p (column row left top right bottom)
  (and
   (<= left column)
   (>= right column)
   (<= top row)
   (>= bottom row)))

;; render map

(defun tiled/render (renderer tiled-map clip-rect)
  "Render map to clip-rect area"
  (let* ((top (sdl2:rect-y clip-rect))
	 (left (sdl2:rect-x clip-rect))
	 (bottom (+ top (sdl2:rect-height clip-rect)))
	 (right (+ left (sdl2:rect-width clip-rect)))
	 (top-left-tile-xy (tiled/get-tile-xy tiled-map left top))
	 (bottom-right-tile-xy (tiled/get-tile-xy tiled-map right bottom))
	 (tile-top (cadr top-left-tile-xy))
	 (tile-left (car top-left-tile-xy))
	 (tile-bottom (cadr bottom-right-tile-xy))
	 (tile-right (car bottom-right-tile-xy))
	 (layers (tiled-map-layers tiled-map))
	 (layer (elt layers 0)) ;; 일단 맨 첫번째 레이어만 가지고 테스트하자
	 (cells (cl-tiled:layer-cells layer)))
    (dolist (cell cells)
      (let ((row (cl-tiled:cell-row cell))
	    (column (cl-tiled:cell-column cell)))
	(when (cell-is-in-p column row tile-left tile-top tile-right tile-bottom)
	  (let* ((gid (cl-tiled:tile-id (cl-tiled:cell-tile  cell)))
		 (tileset-key (get-tileset-key-from-gid tiled-map gid))
		 (texture (tileset-data-texture (gethash tileset-key (tiled-map-atlas-texture-table tiled-map))))
		 (atlas (tileset-data-atlas (gethash tileset-key (tiled-map-atlas-texture-table tiled-map)))))
	    (when (> gid 0)
	      (let ((sprite (make-sprite :texture texture
					 :source-rect (elt atlas gid )
					 :dest-rect (sdl2:make-rect (* column (tiled-map-tile-width tiled-map))
								    (* row (tiled-map-tile-height tiled-map))
								    (tiled-map-tile-width tiled-map)
								    (tiled-map-tile-height tiled-map)))))
		(sprite/render sprite renderer)))))))))
