;;;; file for global variable or constants

(in-package #:dino-lisp)

(defparameter *application-root* (uiop:getcwd))

(defparameter *renderer* nil)
(defparameter *textures* (make-hash-table :test #'equal))
(defparameter *glyphs* (make-hash-table :test #'equal))
(defparameter *tiled-maps* (make-hash-table :test #'equal))
(defparameter *entities* (make-hash-table :test #'equal))
(defparameter *trigger-table* (make-hash-table :test #'equal))
(defparameter *fonts* (make-hash-table :test #'equal))

(defun init-global ()
  (setf *renderer* nil)
  (clrhash *textures*)
  (clrhash *tiled-maps*)
  (clrhash *entities*)
  (clrhash *trigger-table*)
  (clrhash *glyphs*)
  (clrhash *fonts*))
