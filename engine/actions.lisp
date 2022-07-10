(in-package #:dino-lisp)

;; teleport
(defun action/teleport (map col row)
  (lambda (trigger entity)
    (entity/teleport entity map col row)))
