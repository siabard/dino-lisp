(in-package #:dino-lisp)

(defclass progress-bar (gui)
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

(defun make-progress-bar (texture &key (x 0) (y 0) w h atlas (value 100) (max-value 100))
  (make-instance 'progress-bar
		 :x x
		 :y y
		 :w w
		 :h h
		 :texture texture
		 :atlas atlas
		 :value value
		 :max-value max-value))

(defmethod render-width ((gui progress-bar))
  (w gui))

(defmethod render-height ((gui progress-bar))
  (h gui))


;; progress-bar는 외곽과 내측으로 나뉨
;; 가로로 atlas는 6개, 세로는 3개의 크기를 가짐
;; 내측의 폭은 w - (2 * 2), 높이는 h - (2 * 2) 이다.
;; 내측의 길이 값은 iw * (value / max-value) 이다.
(defmethod render-gui ((gui progress-bar) renderer)
  (let* ((atlas (atlas gui))
	 (width (w gui))
	 (height (h gui))
	 (value (value gui))
	 (max-value (max-value gui))
	 (x (x gui))
	 (y (y gui))
	 (texture (texture gui))
	 (atlas-o-nw (elt atlas 0))
	 (atlas-o-n  (elt atlas 1))
	 (atlas-o-ne (elt atlas 2))
	 (atlas-o-w  (elt atlas 6))
	 (atlas-o-e  (elt atlas 8))
	 (atlas-o-sw (elt atlas 12))
	 (atlas-o-s  (elt atlas 13))
	 (atlas-o-se (elt atlas 14))
	 (atlas-i-nw (elt atlas 3))
	 (atlas-i-n  (elt atlas 4))
	 (atlas-i-ne (elt atlas 5))
	 (atlas-i-w  (elt atlas 9))
	 (atlas-i-m  (elt atlas 10))
	 (atlas-i-e  (elt atlas 11))
	 (atlas-i-sw (elt atlas 15))
	 (atlas-i-s  (elt atlas 16))
	 (atlas-i-se (elt atlas 17))
	 (inner-width (- width 4))
	 (inner-height (- height 4))
	 (inner-fill-width (floor  (* inner-width (/ value max-value)))))
    ;; 외측 항목 그리기
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-nw
			 :dest-rect (sdl2:make-rect x y 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-n
			 :dest-rect (sdl2:make-rect (+ x 2) y (- width 4) 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-ne
			 :dest-rect (sdl2:make-rect (- (+ x width) 2) y 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-w
			 :dest-rect (sdl2:make-rect x (+ y 2) 2 (- height 4)))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-e
			 :dest-rect (sdl2:make-rect (- (+ x width) 2) (+ y 2) 2 (- height 4)))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-sw
			 :dest-rect (sdl2:make-rect x (- (+ y height) 2) 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-s
			 :dest-rect (sdl2:make-rect (+ x 2) (- (+ y height) 2) (- width 4) 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-o-se
			 :dest-rect (sdl2:make-rect (- (+ x width) 2) (- (+ y height) 2) 2 2))
    ;; 내측 항목 그리기
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-nw
			 :dest-rect (sdl2:make-rect (+ x 2) (+ y 2) 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-n
			 :dest-rect (sdl2:make-rect (+ x 4) (+ y 2) (- inner-fill-width 4) 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-ne
			 :dest-rect (sdl2:make-rect (- (+ x 2 inner-fill-width) 2) (+ y 2) 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-w
			 :dest-rect (sdl2:make-rect (+ x 2) (+ y 4) 2 (- inner-height 4)))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-e
			 :dest-rect (sdl2:make-rect (- (+ x 2 inner-fill-width) 2) (+ y 4) 2 (- inner-height 4)))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-sw
			 :dest-rect (sdl2:make-rect (+ x 2) (- (+ y 2 inner-height) 2) 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-s
			 :dest-rect (sdl2:make-rect (+ x 4) (- (+ y 2 inner-height) 2) (- inner-fill-width 4) 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-se
			 :dest-rect (sdl2:make-rect (- (+ x 2 inner-fill-width) 2) (- (+ y 2 inner-height) 2) 2 2))
    (sdl2:render-copy-ex renderer texture
			 :source-rect atlas-i-m
			 :dest-rect (sdl2:make-rect (+ x 4) (+ y 4) (- inner-fill-width 4) (- inner-height 4)))
    ))
