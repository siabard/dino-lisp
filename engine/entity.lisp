(in-package #:dino-lisp)


;; TODO Tween 으로 dx, dy 를 감쇄시킬 것
;; tween 객체를 생성하고, 시간이 0이 되면 그냥 nil로 만들까?
(defstruct tween start end timespan current-time)

;; t : 0~1
(defun tween/linear (tw)
  (let* ((start (tween-start tw))
	 (end   (tween-end tw))
	 (ct    (tween-current-time tw))
	 (ts    (tween-timespan tw))
	 (ratio (cond ((= 0 ts) 0)
		      (t  (/ ct ts))))
	 (dt    (cond ((>= ratio 1) 1)
		      (t ratio))))
    (floor (+ (* (- 1  dt) start)
	      (* dt end)))))


;; tween 종료?
(defun tween/end-p (tw)
  (let ((current-time (tween-current-time tw))
	(timespan (tween-timespan tw)))
    (>=  current-time timespan)))


;; time 증가
;; 종료확인하여 종료되면 tween 멈춤
(defun tween/update-dt (tw dt)
  (incf (tween-current-time tw) dt)
  (when (tween/end-p tw)
    (setf (tween-start tw) 0)
    (setf (tween-end tw) 0)
    (setf (tween-timespan tw) 0)
    (setf (tween-current-time tw) 0)))

;; 이동 감쇄량
(defconstant +movement-friction+ 20)


(defstruct entity
  texture width height atlas x y
  dx dy
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

;; dt의 변화에 따른 entity내 변화
(defun entity/increase-elapsed-time-dt (entity dt)
  (let* ((elapsed-time (entity-elapsed-time entity))
	 (next-elapsed-time (+ elapsed-time dt))
	 (animation-span (entity-animation-span entity)))
    (cond ((> next-elapsed-time animation-span)
	   (progn
	     (setf (entity-elapsed-time entity) 0)
	     (entity/increase-current-frame entity)))
	  (t (setf (entity-elapsed-time entity) next-elapsed-time)))))


(defun entity/update-dt (entity dt)
  (entity/increase-elapsed-time-dt entity dt))


(defun entity/update-input (entity keys mouses)
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
    (decf (entity-x entity) 4))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
    (incf (entity-x entity) 4))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
    (decf (entity-y entity) 4))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
    (incf (entity-y entity) 4)))


;; dx, dy 값에 따라 x, y 값을 변화시킨다.
;; dx, dy 는 자연적으로 감소해야한다.
(defun entity/update-dt-tween (entity dt)
  (let ((delta-x (tween/linear (entity-dx entity)))
	(delta-y (tween/linear (entity-dy entity))))
    (incf (entity-x entity) delta-x)
    (incf (entity-y entity) delta-y)
    (tween/update-dt (entity-dx entity) dt)
    (tween/update-dt (entity-dy entity) dt)
    (entity/increase-elapsed-time-dt entity dt)))

;; 키보드 값이 눌리면 dx, dy 값을 변화시킨다.
;; 최대값은 400이다.
(defun entity/update-input-tween (entity keys mouses)
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-left))
    (decf (tween-end (entity-dx entity)) 4)
    (setf (tween-timespan (entity-dx entity)) 100)
    (setf (tween-current-time (entity-dx entity)) 0))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-right))
    (incf (tween-end (entity-dx entity)) 4)
    (setf (tween-timespan (entity-dx entity)) 100)
    (setf (tween-current-time (entity-dx entity)) 0))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-up))
    (decf (tween-end (entity-dy entity)) 4)
    (setf (tween-timespan (entity-dx entity)) 100)
    (setf (tween-current-time (entity-dx entity)) 0))
  (when (key-held-p keys (sdl2:scancode-key-to-value :scancode-down))
    (incf (tween-end (entity-dy entity)) 4)
    (setf (tween-timespan (entity-dx entity)) 100)
    (setf (tween-current-time (entity-dx entity)) 0))
  (cond ((< 40 (tween-end (entity-dx entity))) (setf (tween-end (entity-dx entity)) 40))
	((> -40 (tween-end  (entity-dx entity))) (setf (tween-end  (entity-dx entity)) -40)))
  (cond ((< 40 (tween-end  (entity-dy entity))) (setf (tween-end  (entity-dy entity)) 40))
	((> -40 (tween-end  (entity-dy entity))) (setf (tween-end  (entity-dy entity)) -40))))

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
