(in-package #:dino-lisp)

(defstruct action-def trigger action)

;; teleport

(defun action/teleport (map col row)
  (lambda (trigger entity)
    (entity/teleport entity map col row)))
