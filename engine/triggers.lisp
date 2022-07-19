(in-package #:dino-lisp)

(defstruct triggers trigger-type action hash-data)

(defun trigger/add-action (trigger-table trigger-id  ttype action hash-data)
  (let ((the-trigger (gethash trigger-id trigger-table)))
    (when (not the-trigger) (setf the-trigger (make-triggers)))
    (setf (triggers-trigger-type the-trigger) ttype)
    (setf (triggers-action       the-trigger) action)
    (setf (triggers-hash-data    the-trigger) hash-data)
    (setf (gethash trigger-id trigger-table) the-trigger)))

(defun trigger/add-enter-action (trigger-table x y action)
  (let ((trigger-id (concatenate 'string "enter-" (write-to-string x) "-" (write-to-string y)))
	(hash-data (make-hash-table :test #'equal)))
    (setf (gethash "x" hash-data) x)
    (setf (gethash "y" hash-data) y)
    (trigger/add-action trigger-table trigger-id "enter" action hash-data)))

(defun trigger/add-use-action (trigger-table item-id action)
  (let ((trigger-id (concatenate 'string "use-" (write-to-string item-id)))
	(hash-data (make-hash-table :test #'equal)))
    (setf (gethash "item-id" hash-data) item-id)
    (trigger/add-action trigger-table trigger-id "use" action hash-data)))


(defun trigger/get-enter-action (trigger-table x y)
  (let* ((trigger-id (concatenate 'string "enter-" (write-to-string x) "-"  (write-to-string y)))
	 (trigger    (gethash trigger-id trigger-table)))
    (when trigger
      (triggers-action trigger))))


(defun trigger/get-enter-action-with-map-and-entity (trigger-table tiled-map entity)
  (let* ((x (entity-x entity))
	 (y (entity-y entity))
	 (position (tiled/coord-to-tile-position tiled-map x y)))
    (trigger/get-enter-action trigger-table (car position) (cadr position))))
