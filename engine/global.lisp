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

;; 실제 화면대비 그려야하는 화면 비례값
;; 비례값에 맞추어서 모든 것은 그려져야함.
;; scale-x = physical-width / logical-width
;; scale-y = physical-height / logical-height
;; 모든 texture는 logical-width, logical-height 기준으로 크기가 정렬
;; 실제로 그려져야하는 크기는
;; dest-rect-width = texture-width * scale-x
;; dest-rect-height = texture-height * scale-y

(defparameter *logical-width* 320)
(defparameter *logical-height* 240)

(defparameter *scale-x* 1)
(defparameter *scale-y* 1)
;;;
;;; 현재 게임의 state를 알려주는 전역변수
;;; 현재까지 완료된 내역들이나 통과한 정보들이 들어있게된다.
(defparameter *game-progress* nil)

;;; 존재하는 퀘스트 내역
(defparameter *quests* (make-hash-table :test #'equal))

;;; quest 목록을 저장한다.
(defparameter *quest-log* (make-hash-table :test #'equal))

;; dialog db
(defparameter *dialog-db* (make-hash-table :test #'equal))


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
