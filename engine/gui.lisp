(in-package #:dino-lisp)

;; panel

;; panel은 9개의 tile로 이루어진 집합으로
;; 네 모서리(Top Left, Top Right, Bottom Left, Bottom Right)는 1:1로 출력되고
;; 네 변은 Panel의 가로/세로에서 tile의 가로/세로를 두 배한 만큼 뺀 크기를 가진다.
;; 해당 TILE을 노출할 때 src_rect -> dest_rect 를 변환하는 작업을 거치면 된다.

(defstruct panel-struct texture atlas atlas-width atlas-height tween)

(defun panel/setup (texture width height)
  (let* ((panel (make-panel-struct
		 :texture texture
		 :atlas-width width
		 :atlas-height height
		 :tween (make-tween
			 :start 0
			 :end 1
			 :timespan 50
			 :current-time 0
			 :running t)))
	 (atlas (make-tile-atlas texture width height)))
    (setf (panel-struct-atlas panel) atlas)
    panel))


(defun panel/draw-partial (renderer texture source-rect dest-rect)
  (let ((sprite (make-sprite :texture texture
			     :source-rect source-rect
			     :dest-rect dest-rect)))
    (sprite/render sprite renderer)))

(defun panel/draw-partial-gradient (renderer dest-rect start-color end-color)
  (let ((texture (create-gradient-texture renderer
					  (sdl2:rect-width dest-rect)
					  (sdl2:rect-height dest-rect)
					  start-color
					  end-color)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (sdl2:render-copy-ex renderer
			 texture
			 :source-rect (sdl2:make-rect 0 0 (sdl2:rect-width dest-rect) (sdl2:rect-height dest-rect))
			 :dest-rect dest-rect
			 :angle 0
			 :center (sdl2:make-point 0 0)
			 :flip nil)
    (sdl2:destroy-texture texture)))

(defun panel/update (panel dt)
  (let ((tween (panel-struct-tween panel)))
    (when tween (tween/update-dt tween dt))))

(defun panel/tween-xywh (panel x y w h)
  (let* ((center-x (+ x (/ w 2)))
	 (center-y (+ y (/ h 2)))
	 (tween (panel-struct-tween panel)))
    (cond (tween
	   (let* ((tween-factor (tween/easy-in-circle tween))
		  (tweened-width  (floor  (* w tween-factor)))
		  (tweened-height (floor  (* h tween-factor)))
		  (tweened-x      (floor  (- center-x (* tween-factor (/ w 2)))))
		  (tweened-y      (floor  (- center-y (* tween-factor  (/ w 2))))))
	     (values tweened-x tweened-y tweened-width tweened-height)))
	  (t (values x y w h)))))

(defun panel/render (panel renderer x y w h)
  (multiple-value-bind (x y w h) (panel/tween-xywh panel x y w h)
    (let* ((panel-texture     (panel-struct-texture panel))
	   (panel-atlas       (panel-struct-atlas panel))
	   (panel-top-left    (nth 0 panel-atlas))
	   (panel-top-mid     (nth 1 panel-atlas))
	   (panel-top-right   (nth 2 panel-atlas))
	   (panel-mid-left    (nth 3 panel-atlas))
	   (panel-mid-mid     (nth 4 panel-atlas))
	   (panel-mid-right   (nth 5 panel-atlas))
	   (panel-bot-left    (nth 6 panel-atlas))
	   (panel-bot-mid     (nth 7 panel-atlas))
	   (panel-bot-right   (nth 8 panel-atlas))
	   (panel-width       (panel-struct-atlas-width panel))
	   (panel-height      (panel-struct-atlas-height panel))
	   (panel-width-span  (- w (* 2 panel-width)))
	   (panel-height-span (- h (* 2 panel-height))))
      (when (and (> panel-width-span 0) (> panel-height-span))
	(panel/draw-partial renderer panel-texture panel-top-left (sdl2:make-rect x y panel-width panel-height))
	(panel/draw-partial renderer panel-texture panel-top-mid  (sdl2:make-rect (+  x panel-width) y panel-width-span panel-height))
	(panel/draw-partial renderer panel-texture panel-top-right (sdl2:make-rect (+  x panel-width panel-width-span) y panel-width panel-height))
	(panel/draw-partial renderer panel-texture panel-mid-left (sdl2:make-rect x (+  y panel-height) panel-width  panel-height-span))
	;;(panel/draw-partial renderer panel-texture panel-mid-mid  (sdl2:make-rect (+  x panel-width) (+  y panel-height) panel-width-span panel-height-span))
	(panel/draw-partial-gradient renderer
				     (sdl2:make-rect (+  x panel-width)
						     (+  y panel-height)
						     panel-width-span
						     panel-height-span)
				     #xffff00ff
				     #x0000ffff)
	(panel/draw-partial renderer panel-texture panel-mid-right (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height) panel-width panel-height-span))
	(panel/draw-partial renderer panel-texture panel-bot-left (sdl2:make-rect x (+  y panel-height panel-height-span) panel-width panel-height))
	(panel/draw-partial renderer panel-texture panel-bot-mid  (sdl2:make-rect (+  x panel-width)  (+  y panel-height panel-height-span) panel-width-span panel-height))
	(panel/draw-partial renderer panel-texture panel-bot-right (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height panel-height-span) panel-width panel-height))))))


;; textbox
;; textbox 는 Panel + Text의 조합이다.
;; Text는 Panel 과 일정한 크기의 여백을 가진다.

(defstruct textbox text font textscale textpanel textbounds texttexture)

(defun textbox/set-text (textbox renderer text)
  (let* ((font (textbox-font textbox))
	 (texture-surface (sdl2-ttf:render-utf8-solid font text 255 255 255 0))
	 (texture (sdl2:create-texture-from-surface renderer texture-surface)))
    (setf (textbox-texttexture textbox) texture)))

(defun textbox/render (textbox renderer x y w h)
  (let* ((panel (textbox-textpanel textbox)))
    (panel/render panel renderer x y w h)
    (multiple-value-bind (x y w h) (panel/tween-xywh panel x y w h)
      (let* ((texture (textbox-texttexture textbox))
	     (texture-width (sdl2:texture-width texture))
	     (texture-height (sdl2:texture-height texture)))
	(sdl2:render-copy-ex renderer
			     texture
			     :source-rect (sdl2:make-rect 0 0 texture-width texture-height)
			     :dest-rect (sdl2:make-rect x y w h))))))

;; gui
(defclass gui () ())

(defgeneric render-gui (gui renderer)
  (:documentation "rendering gui"))


(defgeneric set-pos-gui (gui new-x new-y)
  (:documentation "set x, y position"))

(defmethod set-pos-gui (gui new-x new-y)
  (setf (x gui) new-x)
  (setf (y gui) new-y))

;; 일반 gui 텍스트 (label)
(defclass label (gui)
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (content :initarg :content
	    :accessor content)))

(defmethod render-gui ((gui label) renderer)
  (draw-string renderer
	       (x gui)
	       (y gui)
	       (list  (content gui))))
;; dialog window
;; 이제는 그럼 texts 자체를 chunk로 만들어서 가지고 다니도록 하자.

(defclass dialog-window (gui) ((x :initarg :x
				  :accessor x)
			       (y :initarg :y
				  :accessor y)
			       (w :initarg :w
				  :accessor w)
			       (h :initarg :h
				  :accessor h)
			       (index :initarg :index
				      :accessor index)
			       (title :initarg :title
				      :accessor title)
			       (texts-length :initarg :texts-length
					     :accessor texts-length)
			       (texts :initarg :texts
				      :accessor texts)))

;;;; 생성자
;;; title 이 있으면... height가 20 (16 + margin 4) 만큼
;;; title 을 출력할 공간을 확보한다.
(defun make-dialog-window (x y w h &key title texts)
  (let* ((chunked-texts (chunk-text texts w h))
	 (texts-length (length chunked-texts))
	 (first-para (car chunked-texts))
	 (full-length (apply #'max 0 (mapcar #'length first-para))))
    (make-instance 'dialog-window
		   :x x
		   :y y
		   :w (+ 16 (* full-length 16))
		   :h (cond ((eq nil title)
			     (+ 16 (* (length first-para) 16)))
			    (t (+ 36 (* (length first-para) 16))))
		   :index 0
		   :title title
		   :texts-length texts-length
		   :texts chunked-texts)))


;;; dialog 내용 바꾸기
(defgeneric set-dialog-window-texts (dialog-window texts w h)
  (:documentation "내용을 바꾸면서 w, h 도 같이 변경 "))


(defmethod set-dialog-window-texts (dialog-window texts w h)
  (let* ((chunked-texts (chunk-text texts w h))
	 (texts-length (length chunked-texts))
	 (full-length (apply #'max 0 (mapcar #'length (car chunked-texts))))
	 (title (title dialog-window)))
    (when (> (* 16 full-length) (- (w dialog-window) 16))
      (setf (w dialog-window) (+ 16 (* 16 full-length))))
    (when (> (* 16 (length texts)) (- (h dialog-window) 16))
      (setf (h dialog-window)
	    (cond ((eq title nil) (+ 16 (* 16 (length texts))))
		  (t (+ 36 (* 16 (length texts)))))))
    (setf (index dialog-window) 0)
    (setf (texts dialog-window) chunked-texts)
    (setf (texts-length dialog-window) texts-length)))

;;;; rendering 하기
(defgeneric render-dialog-window (dialog-window &key renderer)
  (:documentation "render dialog window"))

(defmethod render-dialog-window (dialog-window &key renderer)
  (let* ((x (x dialog-window))
	 (y (y dialog-window))
	 (w (w dialog-window))
	 (h (h dialog-window))
	 (title (title dialog-window))
	 (texts (texts dialog-window))
	 (current-texts (elt texts (index dialog-window))))
    ;; 외곽선 긋기
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-fill-rect renderer (sdl2:make-rect x y w h))
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-draw-rect renderer (sdl2:make-rect (+ x 4) (+ y 4) (- w 8) (- h 8)))
    ;; 타이틀 있으면 타이틀 쓰기
    (when title
      (draw-string renderer (+ x 8) (+ y 8) title))
    ;; 내용 쓰기
    (dolist (text current-texts)
      (draw-string renderer
		   (+ x 8)
		   (+ y
		      (cond ((eq nil title) 8)
			    (t 28)))
		   text)
      (incf y 16))))

(defmethod render-gui ((gui dialog-window) renderer)
  (render-dialog-window gui :renderer renderer))

;; 클릭시 처리
(defgeneric onclick-dialog-window (dialog-window callback)
  (:documentation "method for onclick"))

(defmethod onclick-dialog-window (dialog-window callback)
  (funcall callback dialog-window))

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
				 (columns 1)
				 (focus-x 0)
				 (focus-y 0)
				 (spacing-x 128)
				 (spacing-y 24)
				 (show-cursor-p T)
				 rows
				 (display-start 0)
				 display-rows
				 (on-selection (lambda () ))
				 (render-choice-item (lambda () )))
  (let* ((max-rows (cond (rows rows)
			 (t (length datasource))))
	 (display-rows (cond (display-rows display-rows)
			     (t max-rows))))
    (make-instance 'choice-dialog
		   :x x :y y
		   :item-width item-width :item-height item-height
		   :focus-x focus-x :focus-y focus-y
		   :spacing-x spacing-x :spacing-y spacing-y
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
	 (focus-x   (focus-x gui))
	 (focus-y   (focus-y gui))
	 (display-start (display-start gui))
	 (display-end (+ display-start (* (display-rows gui))))
	 (cursor      (cursor gui))
	 (cursor-width (sdl2:texture-width (sprite-texture cursor)))
	 (cursor-height (sdl2:texture-height (sprite-texture cursor)))
	 (item-width  (item-width gui))
	 (item-height (item-height gui))
	 (pos-x 0)
	 (pos-y 0))
    (loop for item-index from display-start to display-end
	  do (let* ((item (elt (datasource gui) item-index))
		    (pos (- item-index display-start)))
	       (when (and (> pos 0)
			  (= 0 (rem pos (columns gui))))
		 (setf pos-x 0)
		 (incf pos-y))
	       (when (and (= pos-x focus-x)
			  (= pos-y focus-y))
		 (sprite/set-dest-rect cursor (sdl2:make-rect
					       (+ origin-x (* pos-x
							      (+ cursor-width item-width)))
					       (+ origin-y (* pos-y item-height))
					       cursor-width
					       cursor-height))
		 (sprite/set-source-rect cursor (sdl2:make-rect 0 0 cursor-width cursor-height))
		 (sprite/render cursor renderer))
	       (let* ((dest-x (+ origin-x cursor-width (* pos-x (+ cursor-width item-width))))
		      (dest-y (+ origin-y (* pos-y item-height))))
		 (set-pos-gui item dest-x dest-y)
		 (render-gui item renderer))
	       (incf pos-x)))))
