(in-package #:dino-lisp)

(defclass camera ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (w :initarg :w
      :accessor w)
   (h :initarg :h
      :accessor h)))

(defgeneric get-rect (camera)
  (:documentation "return sdl2 rect"))


(defmethod get-rect (camera)
  (sdl2:make-rect (x camera)
		  (y camera)
		  (w camera)
		  (h camera)))
