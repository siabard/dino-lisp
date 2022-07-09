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

(defstruct tiled-map layers atlas-texture-table width height tile-width tile-height cam-x cam-y)

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
			       :atlas-texture-table atlas-texture-table
			       :cam-x 0
			       :cam-y 0))))
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
(defun tiled/coord-to-tile-position (tiled-map x y)
  (let ((tile-width (tiled-map-tile-width tiled-map))
	(tile-height (tiled-map-tile-height tiled-map)))
    (list (floor x tile-width) (floor y tile-height))))

;; translate map tile position to coordinate
(defun tiled/tile-position-to-coord (tiled-map col row)
  (let ((tile-width (tiled-map-tile-width tiled-map))
	(tile-height (tiled-map-tile-height tiled-map)))
    (list (* tile-width col) (* tile-height row))))

;; get tile layer's value
(defun tiled/cell-at-xy-in (tiled-map layer x y)
  (let* ((position  (tiled/coord-to-tile-position tiled-map x y))
	 (col       (car  position))
	 (row       (cadr position))
	 (layers    (tiled-map-layers tiled-map))
	 (map-layer (find-if (lambda (l) (equal (cl-tiled:layer-name l) layer)) layers)))
    (when map-layer
      (let* ((cells (cl-tiled:layer-cells map-layer))
	     (row-cells (remove-if-not (lambda (cell) (= row (cl-tiled:cell-row cell))) cells))
	     (col-cells (cond (row-cells  (remove-if-not (lambda (cell) (= col (cl-tiled:cell-column cell))) row-cells))
			      (t nil))))
	col-cells))))


;; get hash key from gid
;; layer에는 1부터 시작되므로 이를 줄여야한다.
(defun get-tileset-key-from-gid (tiled-map layer-gid)
  (let* ((first-gids (loop for k being the hash-keys in (tiled-map-atlas-texture-table tiled-map) using (hash-value v)
			   collect (list k (tileset-data-first-gid v))))
	 (gids (mapcar #'cadr first-gids))
	 (filtered-gids (remove-if-not (lambda (elt) (<= elt layer-gid)) gids))
	 (found-first-gid (apply #'max filtered-gids))
	 (filtered-tileset-key (remove-if-not (lambda (lst) (= found-first-gid (cadr lst))) first-gids)))
    filtered-tileset-key))

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
  (let* ((layers (tiled-map-layers tiled-map)) )
    (dolist (layer layers)
      (unless (search "collision----" (cl-tiled:layer-name layer))
	(tiled/render-layer renderer tiled-map layer clip-rect)))))

;; map의 cam 좌표 이동하기
(defun tiled/goto (tiled-map x y)
  (setf (tiled-map-cam-x tiled-map) x)
  (setf (tiled-map-cam-y tiled-map) y))


;; key 설정

(defun tiled/update-keys (tiled-map keys)
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
    (decf (tiled-map-cam-x tiled-map)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
    (incf (tiled-map-cam-x tiled-map)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
    (decf (tiled-map-cam-y tiled-map)))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
    (incf (tiled-map-cam-y tiled-map))))

(defun tiled/render-layer (renderer tiled-map layer clip-rect)
  (let* ((cells (cl-tiled:layer-cells layer))
	 (top (sdl2:rect-y clip-rect))
	 (left (sdl2:rect-x clip-rect))
	 (bottom (+ top (sdl2:rect-height clip-rect)))
	 (right (+ left (sdl2:rect-width clip-rect)))
	 (top-left-tile-xy (tiled/coord-to-tile-position tiled-map left top))
	 (bottom-right-tile-xy (tiled/coord-to-tile-position  tiled-map right bottom))
	 (tile-top (cadr top-left-tile-xy))
	 (tile-left (car top-left-tile-xy))
	 (tile-bottom (cadr bottom-right-tile-xy))
	 (tile-right (car bottom-right-tile-xy)))
    (dolist (cell cells)
      (let ((row (cl-tiled:cell-row cell))
	    (column (cl-tiled:cell-column cell)))
	(when (cell-is-in-p column row tile-left tile-top tile-right tile-bottom)
	  (let* ((gid (cl-tiled:tile-id (cl-tiled:cell-tile  cell)))
		 (tileset-result (get-tileset-key-from-gid tiled-map (+  gid 1)))
		 (tileset-key (caar tileset-result))
		 (tileset-first-gid (cadar tileset-result))
		 (texture (tileset-data-texture (gethash tileset-key (tiled-map-atlas-texture-table tiled-map))))
		 (atlas (tileset-data-atlas (gethash tileset-key (tiled-map-atlas-texture-table tiled-map)))))
	    (when (> gid 0)
	      (let ((sprite (make-sprite :texture texture
					 :source-rect (elt atlas (+ 1 (- gid tileset-first-gid)) )
					 :dest-rect (sdl2:make-rect (-  (* column (tiled-map-tile-width tiled-map))
									(tiled-map-cam-x tiled-map))
								    (-  (* row (tiled-map-tile-height tiled-map))
									(tiled-map-cam-y tiled-map))
								    (tiled-map-tile-width tiled-map)
								    (tiled-map-tile-height tiled-map)))))
		(sprite/render sprite renderer)))))))))


;; Clipping x y according to tiled-map
(defun tiled/clip-xy (tiled-map x y w h)
  (let* ((cam-margin 64)
	 (cam-width  w)
	 (cam-height h)
	 (cam-left   (tiled-map-cam-x tiled-map))
	 (cam-top    (tiled-map-cam-y tiled-map))
	 (cam-right  (+ cam-left cam-width))
	 (cam-bottom (+ cam-top  cam-height))
	 (cam-left-margin   (+ cam-left   cam-margin))
	 (cam-top-margin    (+ cam-top    cam-margin))
	 (cam-right-margin  (- cam-right  cam-margin))
	 (cam-bottom-margin (- cam-bottom cam-margin))
	 (map-width (* (tiled-map-width tiled-map) (tiled-map-tile-width tiled-map)))
	 (map-height (* (tiled-map-height tiled-map) (tiled-map-tile-height tiled-map)))
	 (result-x cam-left)
	 (result-y cam-top))
    (when (< x cam-left-margin)   (decf result-x (- cam-left-margin x)))
    (when (> x cam-right-margin)  (incf result-x (- x cam-right-margin)))
    (when (< y cam-top-margin)    (decf result-y (- cam-top-margin y)))
    (when (> y cam-bottom-margin) (incf result-y (- y cam-bottom-margin)))
    (values (cond ((< result-x 0) 0)
		  ((> result-x (- map-width  w)) (- map-width  w))
		  (t result-x))
	    (cond ((< result-y 0) 0)
		  ((> result-y (- map-height h)) (- map-height h))
		  (t result-y)))))


(defun tiled/destroy-texture (tiled-map)
  (let ((texture-table (tiled-map-atlas-texture-table tiled-map)))
    (loop for k being the hash-keys in texture-table using (hash-value v)
	  do (let ((texture (tileset-data-texture v)))
	       (sdl2:destroy-texture texture)))))

