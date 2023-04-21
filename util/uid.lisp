(in-package #:dino-lisp)

(defun generate-id (prefix)
  (concatenate 'string prefix (symbol-name (gensym))))


(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))
