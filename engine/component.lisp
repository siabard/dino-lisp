(in-package #:dino-lisp)

(defstruct movable speed max-speed x y dx dy new-x new-y)

(defstruct animatable
  animation-span
  animation-map
  current-animation
  current-frame
  elapsed-time )

(defstruct collidable rect channel)

(defstruct renderable texture width height atlas)

;;; typep를 이용해서 타입의 동일성을 검증할 수 있다.
;;; (typep (make-movable) 'movable) -> T
;;; (typep (make-movable) 'animatable) -> T
