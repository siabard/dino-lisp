(in-package #:dino-lisp)

(defun generate-id (prefix)
  (concatenate 'string prefix (symbol-name (gensym))))
