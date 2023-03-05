(in-package #:dino-lisp)

(defparameter *swank-server* nil)

(defun start-swank ()
  (setf *swank-server* (swank:create-server)))

(defun stop-swank ()
  (swank:stop-server *swank-server*))

;; c로 메모리 접근을 하는 함수
(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))


;; core 를 생성하는 함수
(defun save-core ()
  (sb-ext:save-lisp-and-die #p"target/core_latest" :executable t))
