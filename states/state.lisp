(in-package #:dino-lisp)

(defclass game-state ()
  ((scenes :initarg :scenes
	   :accessor scenes)))


(defgeneric update-state (game-state dt &key keyboard mouse)
  (:documentation "State update"))

(defgeneric render-state (game-state renderer)
  (:documentation "Render state"))


(defmethod update-state (game-state dt &key keyboard mouse)
  (dolist (scene scenes)
    (update-scene scene dt :keyboard keyboard :mouse mouse)))
