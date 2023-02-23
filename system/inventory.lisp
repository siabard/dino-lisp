(in-package #:dino-lisp)

;;;; 인벤토리 체계를 만듦 인벤토리는 게임내 생성되는 모든 아이템을
;;;; 담는 item-db와 해당하는 아이템을 entity에서 보관하게해주는
;;;; inventory 체계로 구분할 수 있다.

(defparameter *item-db* (make-hash-table :test #'equal))

;;; item 은 uuid 를 통해 구분되며,
;;; 각기 타입(wearable, consumable, enhancer), 속성, 수명이 할당된다.
;;;
;;; 그렇다면 item으로 얻어진 속성의 보정값(calibration)은 어떻게
;;; 계산되는가?  모든 속성 형식의 값을 누적함으로서 구해진다.
;;;
;;; 속성값에 대한 설계는 아래와 같다.
;;;
;;; 속성 = [{속성타입 : 속성값}]
;;;
;;; 즉 정해진 속성타입에 매치되는 속성값을 누적하면 된다.


;; (defparameter *test-attr* nil)

;; (defparameter *sword* (make-hash-table :test #'equal))
;; (defparameter *armor* (make-hash-table :test #'equal))
;; (defparameter *shild* (make-hash-table :test #'equal))

;; (setf (gethash "attack" *sword*) 10)
;; (setf (gethash "defence" *armor*) 15)
;; (setf (gethash "defence" *shild*) 7)

;; (push *sword* *test-attr*)
;; (push *armor* *test-attr*)
;; (push *shild* *test-attr*)



(defclass item ()
  ((uuid :initarg :uuid
	 :accessor uuid)))


(defun sum-of-hash-table (hashtables)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (item hashtables)
      (maphash (lambda (key value)
		 (let ((current-value (gethash key result)))
		   (setf (gethash key result)
			 (cond (current-value (+ current-value value))
			       (t value)))))
	       item))
    result))
