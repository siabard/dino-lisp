(in-package #:dino-lisp)

;; teleport
(defun action/teleport (map col row)
  (lambda (entity)
    (entity/teleport entity map col row)))
