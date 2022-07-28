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


(defun delete-global-texture ()
  (loop for k being the hash-keys in *textures* using (hash-value v)
	do (safe-delete-texture v))
  (clrhash *textures*))


(defun delete-global-glyphs ()
  (loop for k being the hash-keys in *glyphs* using (hash-value v)
	do (safe-delete-texture v))
  (clrhash *glyphs*))

(defun delete-global-tiled-maps ()
    (loop for k being the hash-keys in *tiled-maps* using (hash-value v)
	do (tiled/destroy-texture v))
  (clrhash *tiled-maps*))


(defun delete-global-entities ()
  (loop for k being the hash-keys in *entities* using (hash-value v)
	do (entity/destroy-texture v))
  (clrhash *entities*))

(defun delete-global-fonts ()
  (loop for k being the hash-keys in *fonts* using (hash-value v)
	do (text/destroy-font v))
  (clrhash *fonts*))

(defun delete-global ()
  (delete-global-glyphs)
  (delete-global-texture)
  (delete-global-tiled-maps)
  (delete-global-entities)
  (delete-global-fonts))
