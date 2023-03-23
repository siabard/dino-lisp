(in-package #:dino-lisp)

;; 일반 gui 텍스트 (label)
(defclass label (gui)
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (content :initarg :content
	    :accessor content)))

(defun make-label (x y text)
  (make-instance 'label :x x :y y :content text))

(defmethod render-gui ((gui label) renderer)
  (draw-string renderer
	       (x gui)
	       (y gui)
	       (content gui)))

(defmethod render-width ((gui label))
  (let ((content (content gui)))
    (* 16 (length content))))

(defmethod render-height ((gui label))
  16)
