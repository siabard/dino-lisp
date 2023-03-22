(in-package #:dino-lisp)

(defclass scroll-bar (gui)
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (w :initarg :w
      :accessor w)
   (h :initarg :h
      :accessor h)
   (texture :initarg :texture
	    :accessor texture)
   (atlas :initarg :atlas
	  :accessor atlas)
   (value :initarg :value
	  :accessor value)
   (max-value :initarg :max-value
	      :accessor max-value)))

(defun make-scroll-bar (texture &key (x 0) (y 0) w h atlas (value 100) (max-value 100))
  (make-instance 'scroll-bar
		 :x x
		 :y y
		 :w w
		 :h h
		 :texture texture
		 :atlas atlas
		 :value value
		 :max-value max-value
		 ))



(defmethod render-width ((gui scroll-bar))
  (w gui))

(defmethod render-height ((gui scroll-bar))
  (h gui))


;; scroll-bar는 4개의 atlas로 이루어짐

;; 상단 이동 버튼
;; dragger
;; 여백
;; 하단 이동 버튼

(defmethod render-gui ((gui scroll-bar) renderer)
  (let* ((atlas (atlas gui))
	 (height (h gui))
	 (value (value gui))
	 (max-value (max-value gui))
	 (x (x gui))
	 (y (y gui))
	 (texture (texture gui))
	 (inner-height (- height 36))
	 (inner-movable-height (- inner-height 18))
	 (dragger-top (floor (* inner-movable-height (/ value max-value))))
	 (top-arrow (elt atlas 0))
	 (down-arrow (elt atlas 3))
	 (dragger (elt atlas 1)))
    ;; top-arrow
    (sdl2:render-copy-ex renderer texture
			 :source-rect top-arrow
			 :dest-rect (sdl2:make-rect x y 18 18))
    ;; down-arrow
    (sdl2:render-copy-ex renderer texture
			 :source-rect down-arrow
			 :dest-rect (sdl2:make-rect x (- (+ y height) 18) 18 18))
    ;; dragger
    (sdl2:render-copy-ex renderer texture
			 :source-rect dragger
			 :dest-rect (sdl2:make-rect x (+ y 18 dragger-top) 18 18))
    ))
