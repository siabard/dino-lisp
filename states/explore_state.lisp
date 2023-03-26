(in-package #:dino-lisp)

(defclass explore-state (game-state)
  ((stack :initarg :stack
	  :accessor stack)
   (mapdata :initarg :mapdata
	    :accessor mapdata)
   (triggers :initarg :triggers
	     :accessor triggers)
   (hero :initarg :hero
	 :accessor hero)
   (camera-width :initarg :camera-width
		 :accessor camera-width)
   (camera-height :initarg :camera-height
		  :accessor camera-height)))


(defun make-explore-state (&key stack mapdata triggers hero camera-width camera-height)
  (make-instance 'explore-state
		 :stack stack
		 :mapdata mapdata
		 :triggers triggers
		 :hero hero
		 :camera-width camera-width
		 :camera-height camera-height))


(defmethod update-state ((state explore-state) dt &key keyboard mouse)
  (let* ((hero (hero state))
	 (mapdata (mapdata state))
	 (triggers (triggers state)))
    (when (and keyboard mouse)
      (entity/update-input hero keyboard mouse))
    (entity/pre-update-dt hero)
    (entity/collide-with-tiled-map hero mapdata)
    (entity/update-dt hero dt)
    (let ((action (trigger/get-enter-action-with-map-and-entity triggers mapdata hero)))
      (when action (funcall action hero)))))


(defmethod handle-input-state ((state explore-state) &key keyboard mouse)
  (entity/update-input (hero state) keyboard mouse))

(defmethod render-state ((state explore-state) renderer)
  (let* ((hero (hero state))
	 (mapdata (mapdata state))
	 (camera-width (camera-width state))
	 (camera-height (camera-height state)))
    (multiple-value-bind (cam-x cam-y)
	(when (entity-movable hero)
	  (tiled/clip-xy mapdata
			 (movable-x (entity-movable hero))
			 (movable-y (entity-movable hero))
			 camera-width
			 camera-height))
      (tiled/goto mapdata cam-x cam-y)
      (tiled/render renderer mapdata (sdl2:make-rect 0 0 camera-width camera-height))
      (entity/render hero renderer cam-x cam-y))))
