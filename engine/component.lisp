(in-package #:dino-lisp)

(defstruct movable speed x y)

(defstruct animatable span animation)

(defstruct collidable rect channel)

;;; typep를 이용해서 타입의 동일성을 검증할 수 있다.
;;; (typep (make-movable) 'movable) -> T
;;; (typep (make-movable) 'animatable) -> T
