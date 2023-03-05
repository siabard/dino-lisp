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
(defparameter *loaded-images* (make-hash-table :test #'equal))
(defparameter *map* (make-hash-table :test #'equal))

;;;
;;; 현재 게임의 state를 알려주는 전역변수
;;; 현재까지 완료된 내역들이나 통과한 정보들이 들어있게된다.
(defparameter *game-progress* nil)


(defparameter *game-state* nil)

;;; 각각의 state
;; 'intro-state     인트로 켜는 상태
;; 'game-state      게임을 진행하는 상태
;; 'dialog-state    대화를 진행중인 상태
;; 'inventory-state 인벤토리 창을 연 상태
;; 'stat-state      상태창을 연 상태
;; 'end-state       게임 오버 화면을 보여주는 상태
;; 'option-state    옵션 설정하는 상태


(defun init-global ()
  (setf *renderer* nil)
  (clrhash *textures*)
  (clrhash *tiled-maps*)
  (clrhash *entities*)
  (clrhash *trigger-table*)
  (clrhash *glyphs*)
  (clrhash *fonts*)
  (clrhash *loaded-images*)
  (clrhash *map*))

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
