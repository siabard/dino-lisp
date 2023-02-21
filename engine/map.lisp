(in-package #:dino-lisp)

;;;; 지도 관련
;;;; 지도를 설정하고
;;;; 지도에 특정 정보를 기록하고
;;;; 지도를 얼마만큼 볼지 기록하고
;;;; 지도를 보여준다.

(defclass gamemap () ((w :initarg :w
			 :accessor w)
		      (h :initarg :h
			 :accessor h)
		      (buffer :initarg :buffer
			      :accessor buffer)))

(defun make-gamemap (w h)
  (make-instance 'gamemap
		 :w w
		 :h h
		 :buffer (make-array (* w h))))


(defgeneric set-map-from (gamemap &key str))

(defmethod set-map-from (gamemap &key str)
  (let ((x 0)
	(y 0))
    (dolist (s str)
      (dolist (c (mapcar #'string  (coerce s 'list)))
	(cond ((string= "#" c)
	       (set-map-xy gamemap
			   :x x
			   :y y
			   :v 1)))
	(incf x))
      (incf y)
      (setf x 0))))

(defgeneric set-map-xy (gamemap &key x y v)
  (:documentation "set map data in x y with value v"))

(defmethod set-map-xy (gamemap &key x y v)
  (setf (aref (buffer gamemap)
	      (+ x (* y (w gamemap))))
	v))

(defgeneric get-map-xy (gamemap &key x y)
  (:documentation "get map data in x y"))

(defmethod get-map-xy (gamemap &key x y)
  (aref (buffer gamemap)
	(+ x (* y (w gamemap)))))


(defgeneric render (gamemap &key renderer)
  (:documentation "render map"))

(defmethod render (gamemap &key renderer)
  (let* ((w (w gamemap))
	 (h (h gamemap)))
    (dotimes (j h)
      (dotimes (i w)
	(when (< 0 (get-map-xy gamemap :x i :y j))
	  (sdl2:set-render-draw-color renderer
				      255
				      255
				      255
				      255)
	  (sdl2:render-fill-rect renderer
				 (sdl2:make-rect (* i 16)
						 (* j 16)
						 16
						 16)))))))
