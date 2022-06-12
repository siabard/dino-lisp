(in-package #:dino-lisp)

;; making mouse system-area-pointer
(defstruct mouse-system x y button-l button-r)

(defun update-mouses (mouses)
  (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
    (setf (mouse-system-x mouses) mouse-x)
    (setf (mouse-system-y mouses) mouse-y)))
