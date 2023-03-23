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
			 :timespan 300
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
		  (tweened-y      (floor  (- center-y (* tween-factor  (/ h 2))))))
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
      (when (and (> panel-width-span 0) (> panel-height-span 0))
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

(defstruct textbox atlas choice title text font textscale textpanel textbounds texttexture)

(defun textbox/set-text (textbox renderer text)
  (let* ((font (textbox-font textbox))
	 (texture-surface (sdl2-ttf:render-utf8-solid font text 255 255 255 0))
	 (texture (sdl2:create-texture-from-surface renderer texture-surface)))
    (setf (textbox-texttexture textbox) texture)))

(defun textbox/update (textbox dt)
  (panel/update (textbox-textpanel textbox) dt))

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

(defgeneric update-gui (gui dt)
  (:documentation "update gui with when elapsed dt"))

(defgeneric render-gui (gui renderer)
  (:documentation "rendering gui"))

(defgeneric render-width (gui)
  (:documentation "rendering width"))

(defgeneric render-height (gui)
  (:documentation "rendering height"))

(defgeneric set-pos-gui (gui new-x new-y)
  (:documentation "set x, y position"))

(defmethod set-pos-gui (gui new-x new-y)
  (setf (x gui) new-x)
  (setf (y gui) new-y))


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
			       (panel :initarg :panel
				      :accessor panel)
			       (index :initarg :index
				      :accessor index)
			       (title :initarg :title
				      :accessor title)
			       (texts-length :initarg :texts-length
					     :accessor texts-length)
			       (texts :initarg :texts
				      :accessor texts)
			       (avatar :initarg :avatar
				       :accessor avatar)
			       (choice :initarg :choice
				       :accessor choice)))

;;;; 생성자
;; cols, rows 가 0 이라면 각각 가능한 전체 크기에 들어갈 수 있는 크기를 사용한다.
;; (*logical-width* - x) / 16, (*logical-height* - y) / 16
;; 일단 화면의 최대 크기를 이용한다. (global의 값 이용)
;; cols ㅁㄴ
(defun make-dialog-window (x y &key (cols (/ (- *logical-width* x) 16)) (rows (/ (- *logical-height* y) 16)) title texts panel avatar choice)
  (let* ((chunked-texts (chunk-text texts cols rows))
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
		   :panel panel
		   :title title
		   :avatar avatar
		   :choice choice
		   :texts-length texts-length
		   :texts chunked-texts)))


;;; update gui

(defmethod update-gui ((gui dialog-window) dt)
  (panel/update (panel gui) dt))

;;; dialog 내용 바꾸기
(defgeneric set-dialog-window-texts (dialog-window texts w h)
  (:documentation "내용을 바꾸면서 w, h 도 같이 변경 "))


(defmethod set-dialog-window-texts (dialog-window texts words lines)
  (let* ((chunked-texts (chunk-text texts words lines))
	 (texts-length (length chunked-texts))
	 (full-length (apply #'max 0 (mapcar #'length (car chunked-texts))))
	 (avatar (avatar dialog-window))
	 (avatar-width (sdl2:rect-width (sprite-dest-rect avatar)))
	 (avatar-height (sdl2:rect-height (sprite-dest-rect avatar)))
	 (choice (choice dialog-window))
	 (title (title dialog-window)))
    (when (> (* 16 full-length) (- (w dialog-window) 16))
      (setf (w dialog-window) (+ 16 (* 16 full-length))))
    (when (> (* 16 (length texts)) (- (h dialog-window) 16))
      (setf (h dialog-window)
	    (cond ((eq title nil) (+ 16 (* 16 (length texts))))
		  (t (+ 36 (* 16 (length texts)))))))
    (when avatar
      (setf (w dialog-window)
	    (+ (w dialog-window)
	       avatar-width))
      (setf (h dialog-window)
	   (max (h dialog-window)
		avatar-height)))
    (when choice
      (setf (h dialog-window)
	    (+ (h dialog-window)
	       (render-height choice))))
    (setf (index dialog-window) 0)
    (setf (texts dialog-window) chunked-texts)
    (setf (texts-length dialog-window) texts-length)))

;;;; rendering 하기
(defgeneric render-dialog-window (dialog-window &key renderer)
  (:documentation "render dialog window"))

(defmethod render-dialog-window (dialog-window &key renderer)
  (let* ((x (+ 0 (x dialog-window)))
	 (y (+ 0 (y dialog-window)))
	 (w (+ 0 (w dialog-window)))
	 (h (+ 0 (h dialog-window)))
	 (title (title dialog-window))
	 (texts (texts dialog-window))
	 (current-texts (elt texts (index dialog-window)))
	 (avatar (avatar dialog-window))
	 (avatar-width (cond (avatar  (sdl2:rect-width (sprite-dest-rect avatar)))
			     (t 0)))
	 (panel (panel dialog-window))
	 (panel-tween-end (tween/end-p (panel-struct-tween (panel dialog-window)))))
    ;; 배경 패널 그리기
    (panel/render panel renderer x y w h)
    ;; 타이틀 있으면 타이틀 쓰기
    (when panel-tween-end
      (when title
	(draw-string renderer (+ x (cond ((eq nil (avatar dialog-window)) 8)
					 (t avatar-width)))
		     (+ y 8) title))
      ;; 아바타 출력하기
      (when avatar
	(sprite/render avatar renderer))
      ;; 내용 쓰기
      (dolist (text current-texts)
	(draw-string renderer
		     (+ x
			(cond ((eq nil (avatar dialog-window)) 8)
			      (t avatar-width)))
		     (+ y
			(cond ((eq nil title) 8)
			      (t 28)))
		     text)
	(incf y 16)))))

(defmethod update-gui ((gui dialog-window) dt)
  (panel/update (panel gui) dt))

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

;; 키 입력처리
(defgeneric process-key-event (gui scancode)
  (:documentation "key event 처리"))

(defmethod process-key-event ((gui choice-dialog) scancode)
  (cond ((sdl2:scancode= scancode
			 :scancode-up)
	 (choice/move-up gui))
	((sdl2:scancode= scancode
			 :scancode-down)
	 (choice/move-down gui))
	((sdl2:scancode= scancode
			 :scancode-left)
	 (choice/move-left gui))
	((sdl2:scancode= scancode
			 :scancode-right)
	 (choice/move-right gui))
	((sdl2:scancode= scancode
			 :scancode-space)
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
