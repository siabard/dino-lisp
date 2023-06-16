(in-package #:dino-lisp)

(defstruct movable  (speed 0) (maxspeed 0) (x 0) (y 0) (dx 0) (dy 0) (new-x 0) (new-y 0))

(defstruct animatable
  animation-span
  animation-map
  current-animation
  current-frame
  elapsed-time )

(defstruct collidable rect channel)

(defstruct renderable texture width height atlas)

(defstruct controlable flag)

;;; typep를 이용해서 타입의 동일성을 검증할 수 있다.
;;; (typep (make-movable) 'movable) -> T
;;; (typep (make-movable) 'animatable) -> T


(defun add-animation (animatable name frames)
  (let ((animation-map (animatable-animation-map animatable)))
    (setf (gethash name animation-map) frames)))
