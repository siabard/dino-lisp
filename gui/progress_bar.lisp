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
   (outer-atlas :initarg :outer-atlas
		:accessor outer-atlas)
   (inner-atlas :initarg :inner-atlas
		:accessor inner-atlas)
   (value :initarg :value
	  :accessor value)))

(defun make-progress-bar (texture)
  )

(defmethod render-width ((gui progress-bar))
  (w gui))

(defmethod render-height ((gui progress-bar))
  (h gui))


;; progress-bar는 외곽과 내측으로 나뉨
(defmethod render-gui ((gui progress-bar) renderer))
