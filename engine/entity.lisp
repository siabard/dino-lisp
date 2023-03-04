(in-package #:dino-lisp)

;;;; Entity 객체 시스템
;;;; TODO 초당 움직여야하는 수치(초당 60픽셀)를 이용해서
;;;; 단위 dt 당 변이값을 갖도록 한다.



(defstruct entity
  controlable
  renderable
  movable
  animatable)


(defun entity/make-entity-atlas (entity)
  (when (entity-renderable entity)
    (let* ((renderable (entity-renderable entity))
	   (texture (renderable-texture renderable))
	   (tile-width (renderable-width renderable))
	   (tile-height (renderable-height renderable))
	   (atlas (make-tile-atlas texture tile-width tile-height)))
      (setf (renderable-atlas renderable) atlas))))


(defun entity/make-animation-map (entity)
  (when (entity-animatable entity)
    (let ((animatable (entity-animatable entity)))
      (setf (animatable-animation-map animatable)
	    (make-hash-table :test #'equal)))))

(defun entity/add-animation (entity name frames)
  (when (entity-animatable entity)
    (let* ((animatable (entity-animatable entity))
	   (animation-map (animatable-animation-map animatable)))
      (setf (gethash name animation-map) frames))))

(defun entity/increase-current-frame (entity)
  (when (entity-animatable entity)
    (let* ((animatable (entity-animatable entity))
	   (current-frame (animatable-current-frame animatable))
	   (animation-map (animatable-animation-map animatable))
	   (current-animation (animatable-current-animation animatable))
	   (animation-frames (gethash current-animation animation-map))
	   (next-frame (+ 1 current-frame))
	   (new-current-frame (cond ((>= next-frame (length animation-frames)) 0)
				    (t next-frame))))
      (setf (animatable-current-frame animatable) new-current-frame))))

;; dt의 변화에 따른 entity내 변화
;; animation 처리
(defun entity/increase-elapsed-time-dt (entity dt)
  (when (entity-animatable entity)
    (let* ((animatable (entity-animatable entity))
	   (elapsed-time (animatable-elapsed-time animatable))
	   (next-elapsed-time (+ elapsed-time dt))
	   (animation-span (animatable-animation-span animatable)))
      (cond ((> next-elapsed-time animation-span)
	     (progn
	       (setf (animatable-elapsed-time animatable) 0)
	       (entity/increase-current-frame entity)))
	    (t (setf (animatable-elapsed-time animatable) next-elapsed-time))))))


(defun entity/update-dt-normal (entity dt)
  (entity/increase-elapsed-time-dt entity dt))


(defun entity/update-input-normal (entity keys mouses)
  (when (entity-movable entity)
    (let ((movable (entity-movable entity)))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
	(decf (movable-x movable) 4))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
	(incf (movable-x movable) 4))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
	(decf (movable-y movable) 4))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
	(incf (movable-y movable) 4)))))


(defun entity/set-current-animation (entity name)
  (when (entity-animatable entity)
    (setf (animatable-current-animation (entity-animatable entity)) name)))

;; 벡터의 내적
(defun inner-product (x y)
  (+ (* x x) (* y y)))

;; animation이 바뀔 때 기존과 다르다면, frame을 0으로 리셋해야한다.
;; dx , dy 가 0 이상이면 일단 walk-left
;; dx , dy 가 0이면 idle

(defun entity/change-animation (entity animation)
  (let ((animatable (entity-animatable entity)))
    (when animatable
      (let ((current-animation (animatable-current-animation animatable)))
	(when (not (equal current-animation animation ))
	  (setf (animatable-current-animation animatable) animation)
	  (setf (animatable-current-frame animatable) 0))))))


;; 현재 프레임에서의 위치를 설정한다.
;; dx, dy 값에 따라 x, y 값을 변화시킨다.

(defun entity/pre-update-dt (entity)
  (when (entity-movable entity)
    (let* ((movable (entity-movable entity))
	   (delta-x (tween/linear (movable-dx movable)))
	   (delta-y (tween/linear (movable-dy movable))))
      (setf (movable-new-x movable) (+ (movable-x movable) delta-x))
      (setf (movable-new-y movable) (+ (movable-y movable) delta-y)))))

;; dx, dy 는 tween에 맞추어 변이한다.
(defun entity/update-dt (entity dt)
  (when (entity-movable entity)
    (let* ((movable (entity-movable entity))
	   (delta-x (tween/linear (movable-dx movable)))
	   (delta-y (tween/linear (movable-dy movable))))
      (tween/update-dt (movable-dx movable) dt)
      (tween/update-dt (movable-dy movable) dt)
      (cond ((> (inner-product delta-x delta-y) 0)
	     (entity/change-animation entity "walk-left"))
	    (t (entity/change-animation entity "idle")))
      (entity/increase-elapsed-time-dt entity dt))))


;; 키보드 값이 눌리면 dx, dy 값을 변화시킨다.
;; 최대값은 400이다.
(defun entity/update-input (entity keys mouses)
  (when (entity-movable entity)
    (let* ((movable   (entity-movable entity))
	   (maxspeed  (movable-maxspeed movable))
	   (-maxspeed (* -1 maxspeed))
	   (dx        (movable-dx movable))
	   (dy        (movable-dy movable)))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
	(setf (tween-running dx) t)
	(decf (tween-start dx)        0.1)
	(setf (tween-timespan dx)     100)
	(setf (tween-current-time dx) 0))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
	(setf (tween-running dx) t)
	(incf (tween-start dx)        0.1)
	(setf (tween-timespan dx)     100)
	(setf (tween-current-time dx) 0))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
	(setf (tween-running dy) t)
	(decf (tween-start dy)        0.1)
	(setf (tween-timespan dy)     100)
	(setf (tween-current-time dy) 0))
      (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
	(setf (tween-running dy) t)
	(incf (tween-start dy)        0.1)
	(setf (tween-timespan dy)     100)
	(setf (tween-current-time dy) 0))
      (cond ((< maxspeed  (tween-start dx)) (setf (tween-start dx)  maxspeed))
	    ((> -maxspeed (tween-start dx)) (setf (tween-start dx) -maxspeed)))
      (cond ((< maxspeed  (tween-start dy)) (setf (tween-start dy)  maxspeed))
	    ((> -maxspeed (tween-start dy)) (setf (tween-start dy) -maxspeed))))))

(defun entity/render (entity renderer cam-x cam-y)
  (when (and (entity-animatable entity)
	     (entity-renderable entity)
	     (entity-movable entity))
    (let* ((movable (entity-movable entity))
	   (renderable (entity-renderable entity))
	   (animatable (entity-animatable entity))
	   (current-frame (animatable-current-frame animatable))
	   (current-animation (animatable-current-animation animatable))
	   (animation-map (animatable-animation-map animatable))
	   (animation-frames (gethash current-animation animation-map))
	   (pos (elt animation-frames current-frame))
	   (source-rect (elt (renderable-atlas renderable) pos))
	   (dest-rect (sdl2:make-rect (-  (movable-x movable) cam-x)
				      (-  (movable-y movable) cam-y)
				      (renderable-width  renderable)
				      (renderable-height renderable))))
      (sdl2:render-copy-ex renderer (renderable-texture renderable)
			   :source-rect source-rect
			   :dest-rect dest-rect))))

(defun entity/destroy-texture (entity)
  (when (entity-renderable entity)
    (safe-delete-texture (renderable-texture (entity-renderable entity)))))


;; map collision 영역과 부딪혔을 때 움직이지못하도록 처리
(defun entity/collide-with-tiled-map (entity tiled-map)
  (when (and (entity-movable entity)
	     (entity-renderable entity))
    (let* ((movable           (entity-movable entity))
	   (renderable        (entity-renderable entity))
	   (entity-left       (movable-new-x movable))
	   (entity-top        (movable-new-y movable))
	   (entity-right      (+ (movable-new-x movable) (renderable-width  renderable)))
	   (entity-bottom     (+ (movable-new-y movable) (renderable-height renderable)))
	   (entity-old-left   (movable-x movable))
	   (entity-old-top    (movable-y movable))
	   (entity-old-right  (+ (movable-x movable) (renderable-width  renderable)))
	   (entity-old-bottom (+ (movable-y movable) (renderable-height renderable)))
	   (collision-top-left  (tiled/cell-at-xy-in tiled-map "collision" entity-left  entity-top))
	   (collision-top-right (tiled/cell-at-xy-in tiled-map "collision" entity-right entity-top))
	   (collision-bottom-left  (tiled/cell-at-xy-in tiled-map "collision" entity-left  entity-bottom))
	   (collision-bottom-right (tiled/cell-at-xy-in tiled-map "collision" entity-right entity-bottom))
	   (collision-old-top-left  (tiled/cell-at-xy-in tiled-map "collision" entity-left  entity-old-top))
	   (collision-old-top-right (tiled/cell-at-xy-in tiled-map "collision" entity-right entity-old-top))
	   (collision-old-bottom-left  (tiled/cell-at-xy-in tiled-map "collision" entity-left  entity-old-bottom))
	   (collision-old-bottom-right (tiled/cell-at-xy-in tiled-map "collision" entity-right entity-old-bottom))
	   (collision-top-old-left  (tiled/cell-at-xy-in tiled-map "collision" entity-old-left  entity-old-top))
	   (collision-top-old-right (tiled/cell-at-xy-in tiled-map "collision" entity-old-right entity-top))
	   (collision-bottom-old-left  (tiled/cell-at-xy-in tiled-map "collision" entity-old-left  entity-bottom))
	   (collision-bottom-old-right (tiled/cell-at-xy-in tiled-map "collision" entity-old-right entity-bottom)))
      (cond ((not (or collision-top-left collision-top-right collision-bottom-left collision-bottom-right))
	     (progn  (setf (movable-x movable) (movable-new-x movable))
		     (setf (movable-y movable) (movable-new-y movable))))
	    ((not (or collision-old-top-left collision-old-top-right collision-old-bottom-left collision-old-bottom-right))
	     (progn (setf (movable-x movable) (movable-new-x movable))
		    (tween/stop (movable-dy movable))))
	    ((not (or collision-top-old-left collision-top-old-right collision-bottom-old-left collision-bottom-old-right))
	     (progn (setf (movable-y movable) (movable-new-y movable))
		    (tween/stop (movable-dx movable))))
	    (t (progn
		 (tween/stop (movable-dx movable))
		 (tween/stop (movable-dy movable))))))))



;; tiled-map 의 (col, row)위치로 entity를 이동시킴
(defun entity/teleport (entity tiled-map col row)
  (when (entity-movable entity)
    (let ((movable (entity-movable entity))
	  (coord (tiled/tile-position-to-coord tiled-map col row)))
      (setf (movable-x movable) (car coord))
      (setf (movable-y movable) (cadr coord)))))

;; 아이템을 갖는다.
;; 실제 item의 정보는 *item-db* 에 있을 것이고
;; 해당 item 정보의 owner에 entity를 넣는다.
(defun entity/get-item (entity uid db)
  (let ((item (gethash uid db)))
    (setf (owner item) entity)
    (setf (map-position item) nil)))

;; 아이템을 잃는다.
(defun entity/lost-item (entity uid db)
  (let ((item (gethash uid db)))
    (setf (owner item) nil)))
