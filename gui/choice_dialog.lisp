(in-package #:dino-lisp)

;; choices

(defclass choice-dialog (gui)
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (item-width :initarg :item-width
	       :accessor item-width)
   (texture :initarg :texture
	    :accessor texture)
   (atlas :initarg :atlas
	  :accessor atlas)
   (item-height :initarg :item-height
		:accessor item-height)
   (datasource :initarg :datasource
	       :accessor datasource)
   (columns :initarg :columns
	    :accessor columns)
   (focus-x :initarg :focus-x
	    :accessor focus-x)
   (focus-y :initarg :focus-y
	    :accessor focus-y)
   (spacing-x :initarg :spacing-x
	      :accessor spacing-x)
   (spacing-y :initarg :spacing-y
	      :accessor spacing-y)
   (cursor :initarg :cursor
	   :accessor cursor)
   (show-cursor-p :initarg :show-cursor-p
		  :accessor show-cursor-p)
   (max-rows :initarg :max-rows
	     :accessor max-rows)
   (display-start :initarg :display-start
		  :accessor display-start)
   (display-rows :initarg :display-rows
		 :accessor display-rows)
   (on-selection :initarg :on-selection
		 :accessor on-selection)
   (render-choice-item :initarg :render-choice-item
		       :accessor render-choice-item)))



(defun make-choice-dialog (x y &key
				 datasource
				 item-width
				 item-height
				 texture
				 atlas
				 cursor
				 (columns 1)
				 (focus-x 0)
				 (focus-y 0)
				 (spacing-x 128)
				 (spacing-y 24)
				 (show-cursor-p T)
				 rows
				 (display-start 0)
				 display-rows
				 (on-selection (lambda (x) nil))
				 (render-choice-item (lambda () )))
  (let* ((max-rows (cond (rows rows)
			 (t (length datasource))))
	 (display-rows (cond (display-rows display-rows)
			     (t max-rows))))
    (make-instance 'choice-dialog
		   :x x :y y
		   :item-width item-width :item-height item-height
		   :datasource datasource
		   :focus-x focus-x :focus-y focus-y
		   :spacing-x spacing-x :spacing-y spacing-y
		   :cursor cursor
		   :show-cursor-p show-cursor-p
		   :max-rows max-rows
		   :columns columns
		   :texture texture
		   :atlas atlas
		   :display-start display-start
		   :display-rows display-rows
		   :on-selection on-selection
		   :render-choice-item render-choice-item)))

(defmethod render-gui ((gui choice-dialog) renderer)
  (let* ((origin-x  (x gui))
	 (origin-y  (y gui))
	 (spacing-x (spacing-x gui))
	 (spacing-y (spacing-y gui))
	 (focus-x   (focus-x gui))
	 (focus-y   (focus-y gui))
	 (max-end   (- (length (datasource gui)) 1))
	 (display-start (display-start gui))
	 (display-end (- (+ display-start (* (display-rows gui) (columns gui))) 1))
	 (display-end (cond ((> display-end max-end)
			     max-end)
			    (t display-end)))
	 (cursor      (cursor gui))
	 (cursor-width (sdl2:texture-width (sprite-texture cursor)))
	 (cursor-height (sdl2:texture-height (sprite-texture cursor)))
	 (item-width  (item-width gui))
	 (item-height (item-height gui))
	 (pos-x 0)
	 (pos-y 0)
	 (show-cursor-p (show-cursor-p gui)))
    (loop for item-index from display-start to display-end
	  do (let* ((item (elt (datasource gui) item-index))
		    (pos (- item-index display-start)))
	       (when (and (> pos 0)
			  (= 0 (rem pos (columns gui))))
		 (setf pos-x 0)
		 (incf pos-y))
	       (when (and show-cursor-p
			  (= pos-x focus-x)
			  (= pos-y focus-y))
		 (sprite/set-dest-rect cursor (sdl2:make-rect
					       (+ origin-x (* pos-x
							      (+ cursor-width item-width spacing-x)))
					       (+ origin-y (* pos-y (+  item-height spacing-y)))
					       cursor-width
					       cursor-height))
		 (sprite/set-source-rect cursor (sdl2:make-rect 0 0 cursor-width cursor-height))
		 (sprite/render cursor renderer))
	       (let* ((dest-x (+ origin-x cursor-width
				 (* pos-x (+ cursor-width item-width spacing-x))))
		      (dest-y (+ origin-y (* pos-y (+  item-height spacing-y)))))
		 (set-pos-gui item dest-x dest-y)
		 (render-gui item renderer))
	       (incf pos-x)))))


;; 셀렉트 상하 이동
(defun choice/move-up (choice)
  (let* ((focus-y (focus-y choice))
	 (columns (columns choice))
	 (display-start (display-start choice)))
    (cond ((> focus-y 0)
	   (setf (focus-y choice) (- focus-y 1)))
	  (t
	   (when (<= columns display-start)
	     (setf (display-start choice) (- display-start columns)))))))

(defun choice/move-down (choice)
  (let* ((focus-y (focus-y choice))
	 (display-rows (display-rows choice))
	 (display-start (display-start choice))
	 (columns (columns choice))
	 (datasource (datasource choice))
	 (next-display-start (+ display-start columns))
	 (next-display-end (- (+ next-display-start (* display-rows columns)) 1))
	 (next-rows (ceiling (/ (+  next-display-end 1) columns)))
	 (total-rows (ceiling (/  (length datasource) columns))))
    (cond ((< focus-y (-  display-rows 1))
	   (setf (focus-y choice) (+ focus-y 1)))
	  (t
	   (when (<= next-rows total-rows)
	     (setf (display-start choice) next-display-start))))))

(defun choice/move-left (choice)
  (let* ((focus-x (focus-x choice)))
    (cond ((> focus-x 0)
	   (setf (focus-x choice) (- focus-x 1))))))

(defun choice/current-index (choice)
  (let* ((focus-x (focus-x choice))
	 (focus-y (focus-y choice))
	 (display-start (display-start choice))
	 (columns (columns choice)))
    (+ display-start focus-x (* focus-y columns))))


(defun choice/move-right (choice)
  (let* ((focus-x (focus-x choice))
	 (current-index (choice/current-index choice))
	 (max-index (-  (length (datasource choice)) 1))
	 (columns (columns choice)))
    (when (and
	   (< focus-x (- columns 1))
	   (> max-index current-index))
      (setf (focus-x choice) (+ focus-x 1)))))



(defun choice/get-index (choice)
  (let ((focus-x (focus-x choice))
	(focus-y (focus-y choice))
	(columns (columns choice)))
    (+ focus-x (* focus-y columns))))

(defun choice/on-click (choice)
  (let ((index (choice/get-index choice))
	(on-selection (on-selection choice))
	(datasource (datasource choice)))
    (funcall on-selection (elt datasource index))))

(defmethod process-key-event ((gui choice-dialog) scancode)
  (cond ((sdl2:scancode= scancode
			 (sdl2:scancode-key-to-value :scancode-up))
	 (choice/move-up gui))
	((sdl2:scancode= scancode
			 (sdl2:scancode-key-to-value :scancode-down))
	 (choice/move-down gui))
	((sdl2:scancode= scancode
			 (sdl2:scancode-key-to-value :scancode-left))
	 (choice/move-left gui))
	((sdl2:scancode= scancode
			 (sdl2:scancode-key-to-value :scancode-right))
	 (choice/move-right gui))
	((sdl2:scancode= scancode
			 (sdl2:scancode-key-to-value :scancode-space))
	 (choice/on-click gui))))

(defmethod render-height ((gui choice-dialog))
  (let ((spacing-y (spacing-y gui))
	(display-rows (display-rows gui)))
    (* spacing-y display-rows)))


(defmethod render-width ((gui choice-dialog))
  (let* ((datasource (datasource gui))
	 (datasource-length (mapcar (lambda (item) (render-width item)) datasource)))
    (apply #'max datasource-length)))


(defun choice/percentage-shown (choice)
  (let ((display-rows (display-rows choice))
	(max-rows (max-rows choice)))
    (/ display-rows max-rows)))

(defun choice/percentage-scrolled (choice)
  (let* ((max-rows (max-rows choice))
	 (focus-y (focus-y choice))
	 (current-percent (/ focus-y max-rows)))
    current-percent))


(defun choice/selected-item (choice)
  (elt (datasource choice) (choice/get-index choice)))
