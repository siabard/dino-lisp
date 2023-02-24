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

;;; 아이템은 entity에 대해 n의 관계이므로
;;; 아이템에는 map이나 entity에 대한 위치 데이터도 필요하다.
;;; owner 에 값이 있다면 대상 entity이며, 없다면 map의 특정 위치에 있는 것이다.
;;; 그러므로 position 과 owner 정보가 복합적으로 필요하다.
(defclass item ()
  ((uuid :initarg :uuid
	 :accessor uuid)
   (category :initarg :category
	     :accessor category)
   (name :initarg :name
	 :accessor name)
   (owner :initarg :owner
	  :accessor owner)
   (map-position :initarg :map-position
		 :accessor map-position)))


(defun make-item (name &key category owner map-position)
  (make-instance 'item
		 :uuid (uuid:make-v4-uuid)
		 :category category
		 :name name
		 :owner owner
		 :map-position map-position))

(defgeneric print-item (item)
  (:documentation "print item infomation"))

(defmethod print-item (item)
  (format t "~A ~A ~A ~A ~%" (uuid item) (name item) (category item) (owner item)))

;;; 보정값을 구하기위해 일련의 아이템 정보를 받아 인벤토리 아이템으로
;;; 얻어지는 이득치를 계산한다.
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

(defun show-item-db (db)
  (maphash (lambda (key value)
	     (print-item value))
	   db))

(defun create-item (db category name)
  (let ((item (make-item name :category category)))
    (setf (gethash (uuid:format-as-urn nil (uuid item)) db) item)))

(defun destroy-item (db item-uuid)
  (remhash (uuid:format-as-urn nil item-uuid) db))
