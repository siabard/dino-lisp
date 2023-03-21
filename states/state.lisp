(in-package #:dino-lisp)

(defclass game-state ()
  ((scenes :initarg :scenes
	   :accessor scenes)))

(defun make-game-state (scenes)
  (make-instance 'game-state :scenes scenes))

(defgeneric update-state (game-state dt &key keyboard mouse)
  (:documentation "State update"))

(defgeneric render-state (game-state renderer)
  (:documentation "Render state"))


(defmethod update-state (game-state dt &key keyboard mouse)
  (dolist (scene scenes)
    (update-scene scene dt :keyboard keyboard :mouse mouse)))


(defmethod render-state (game-state renderer)
  (dolist (scene (scenes game-state))
    (render-scene scene renderer)))