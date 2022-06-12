(in-package #:dino-lisp)

;; KEY related function
;; TODO: should refactoring to class or struct

(defclass key-input () ((pressed :accessor pressed)
			(released :accessor release)
			(held :accessor held)))

;;(defvar *pressed-key* (make-hash-table :test #'equal))
;;(defvar *released-key* (make-hash-table :test #'equal))
;;(defvar *held-keys* (make-hash-table :test #'equal))

(defmethod init-keys ((new-key-input key-input))
  (setf (slot-value new-key-input 'pressed) (make-hash-table :test 'equal))
  (setf (slot-value new-key-input 'released) (make-hash-table :test 'equal))
  (setf (slot-value new-key-input 'held) (make-hash-table :test 'equal)))

(defmethod clear-keys ((keys key-input))
  (clrhash (slot-value keys 'pressed))
  (clrhash (slot-value keys 'released)))

(defmethod keyup-event ((keys key-input) scancode)
  (let ((released (slot-value keys 'released))
	(held (slot-value keys 'held)))
    (setf (gethash scancode released) t)
    (setf (gethash scancode held) nil)))

(defmethod keydown-event ((keys key-input) scancode)
  (let ((pressed (slot-value keys 'pressed))
	(held (slot-value keys 'held)))
    (setf (gethash scancode pressed) t)
    (setf (gethash scancode held) t)))

(defmethod key-pressed-p ((keys key-input) scancode)
  (let ((pressed (slot-value keys 'pressed)))
    (gethash scancode pressed nil)))

(defmethod key-released-p ((keys key-input) scancode)
  (let ((released (slot-value keys 'released)))
    (gethash scancode released nil)))

(defmethod key-held-p ((keys key-input) scancode)
  (let ((held (slot-value keys 'held)))
    (gethash scancode held nil)))

;; //END OF KEY related function
