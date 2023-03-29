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
  (setf (scenes game-state) (remove-if #'terminated (scenes game-state))))


(defmethod render-state (game-state renderer)
  (dolist (scene (scenes game-state))
    (render-scene scene renderer)))


;; gui에서 받은 것
(defmethod update-gui ((gui game-state) dt)
  (update-state gui dt :keyboard nil :mouse nil))


(defmethod render-gui ((gui game-state) renderer)
  (render-gui gui renderer))
