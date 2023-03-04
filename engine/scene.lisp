(in-package #:dino-lisp)

(defclass scene ()
  ((name :initarg :name
	 :accessor name)
   (x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (zoom :initarg :zoom
	 :accessor zoom)
   (width :initarg :width
	  :accessor width)
   (height :initarg :height
	   :accessor height)
   (background :initarg :background
	       :accessor background)
   (guis :initarg :guis
	 :accessor guis)
   (entities :initarg :entities
	     :accessor entities)
   (camera :initarg :camera
	   :accessor camera)))

(defun make-scene (name &key (x 0) (y 0) (zoom 1) (width 100) (height 100) (background nil) (camera nil) (guis nil) (entities nil))
  (make-instance 'scene
		 :name name
		 :x x
		 :y y
		 :width width
		 :height height
		 :zoom zoom
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


;;; 렌더링용 texture을 만들어서
;;; entity, gui 등을 그 위에 그리고,
;;; 그 다음에 이것을 화면에 뿌리게 하기
;;; scene에서만 이렇게하면 되고, 나머지 애들은
;;; 그냥 뿌린다.

;;; 마지막에 출력할 때에는 zoom을 width / height 곱한다.

(defmethod render-scene (scene renderer)
  (let* ((texture (sdl2:create-texture renderer
				       sdl2:+pixelformat-rgba8888+
				       sdl2-ffi:+sdl-textureaccess-target+
				       (width scene)
				       (height scene)))
	 (clip-texture (sdl2:create-texture renderer
				       sdl2:+pixelformat-rgba8888+
				       sdl2-ffi:+sdl-textureaccess-target+
				       (width scene)
				       (height scene)))
	 (background (background scene)))
    (sdl2:set-render-target renderer texture)
    (sdl2:set-render-draw-color renderer 0 0 0 255 )
    (sdl2:render-clear renderer)
    (dolist (gui (guis scene))
      (render-gui gui renderer))
    (dolist (entity (entities scene))
      (entity/render entity
		     renderer
		     (sdl2:rect-x (camera scene))
		     (sdl2:rect-y (camera scene))))
    (when background
      (let ((background-rect (sdl2:make-rect 0
					     0
					     (sdl2:texture-width background)
					     (sdl2:texture-height background))))
	(sdl2:render-copy-ex renderer
			     background
			     :source-rect background-rect
			     :dest-rect (sdl2:make-rect 0 0
							(sdl2:rect-width (camera scene))
							(sdl2:rect-height (camera scene))))))
    (sdl2:set-render-target renderer clip-texture)
    (sdl2:render-copy-ex renderer
			 texture
			 :source-rect (sdl2:make-rect 0 0 (width scene) (height scene))
			 :dest-rect (sdl2:make-rect (x scene)
						    (y scene)
						    (* (zoom scene) (width scene))
						    (* (zoom scene) (height scene))))
    (sdl2:set-render-target renderer nil)
    (sdl2:render-copy-ex renderer
			 clip-texture
			 :source-rect (sdl2:make-rect 0 0 (width scene) (height scene))
			 :dest-rect (sdl2:make-rect (x scene)
						    (y scene)
						    (width scene)
						    (height scene)))

    (sdl2:destroy-texture texture)
    (sdl2:destroy-texture clip-texture)))
