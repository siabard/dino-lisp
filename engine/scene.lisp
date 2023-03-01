(in-package #:dino-lisp)

(defclass scene ()
  ((name :initarg :name
	 :accessor name)
   (background :initarg :background
	       :accessor background)
   (guis :initarg :guis
	 :accessor guis)
   (entities :initarg :entities
	     :accessor entities)
   (camera :initarg :camera
	   :accessor camera)))

(defun make-scene (name &key background entities camera guis)
  (make-instance 'scene
		 :name name
		 :background background
		 :entities entities
		 :guis guis
		 :camera camera))

(defgeneric update-scene (scene dt &key keyboard mouse)
  (:documentation "update all entities in scene"))

(defgeneric render-scene (scene renderer)
  (:documentation "render all entities in scene"))


(defmethod update-scene (scene dt &key keyboard mouse)
  (dolist (entity (entities scene))
    (entity/update-input entity keyboard mouse)
    (entity/pre-update-dt entity)
    (entity/update-dt entity dt)))

(defmethod render-scene (scene renderer)
  (let* ((background (background scene))
	 (background-rect (sdl2:make-rect 0
					  0
					  (sdl2:texture-width background)
					  (sdl2:texture-height background))))
    (dolist (gui (guis scene))
      (render-gui gui renderer))
    (dolist (entity (entities scene))
      (entity/render entity
		     renderer
		     (x (camera scene))
		     (y (camera scene))))
    (sdl2:render-copy-ex renderer
			 background
			 :source-rect background-rect
			 :dest-rect (sdl2:make-rect 0 0  (w (camera scene))
						    (h (camera scene))))))
