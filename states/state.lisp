(in-package #:dino-lisp)

(defclass game-state (gui)
  ((scenes :initarg :scenes
	   :accessor scenes)))

(defun make-game-state (scenes)
  (make-instance 'game-state :scenes scenes))

(defgeneric update-state (game-state dt &key keyboard mouse)
  (:documentation "State update"))

(defgeneric render-state (game-state renderer)
  (:documentation "Render state"))

(defgeneric handle-input-state (game-state &key mouse keyboard)
  (:documentation "handle input in states"))

(defgeneric enter-state (game-state)
  (:documentation "entering state (load)"))

(defgeneric exit-state (game-state)
  (:documentation "exit state (unload)"))

(defmethod update-state (game-state dt &key keyboard mouse)
  (dolist (scene (scenes game-state))
    (update-scene scene dt :keyboard keyboard :mouse mouse)
    (when (blocked scene)
      (return)))
  (setf (scenes game-state)
	(remove-if (lambda (scene) (terminated scene))
		   (scenes game-state))))


(defmethod render-state (game-state renderer)
  (dolist (scene (scenes game-state))
    (render-scene scene renderer)))
