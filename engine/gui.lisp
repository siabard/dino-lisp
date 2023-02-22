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

;; dialog window
(defclass dialog-window () ((x :initarg :x
			       :accessor x)
			    (y :initarg :y
			       :accessor y)
			    (w :initarg :w
			       :accessor w)
			    (h :initarg :h
			       :accessor h)
			    (texts :initarg :texts
				   :accessor texts)))

;;;; 생성자
(defun make-dialog-window (x y texts)
  (let ((full-length (apply #'max 0 (mapcar #'length texts))))
    (make-instance 'dialog-window
		   :x x
		   :y y
		   :w (+ 16 (* full-length 16))
		   :h (+ 16 (* (length texts) 16))
		   :texts texts)))


;;;; rendering 하기
(defgeneric render-dialog-window (dialog-window &key renderer)
  (:documentation "render dialog window"))

(defmethod render-dialog-window (dialog-window &key renderer)
  (let* ((x (x dialog-window))
	 (y (y dialog-window))
	 (w (w dialog-window))
	 (h (h dialog-window))
	 (texts (texts dialog-window)))
    ;; 외곽선 긋기
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-fill-rect renderer (sdl2:make-rect x y w h))
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-draw-rect renderer (sdl2:make-rect (+ x 4) (+ y 4) (- w 8) (- h 8)))
    ;; 내용 쓰기
    (dolist (text texts)
      (draw-string renderer (+ x 8) (+ y 8) text)
      (incf y 16))))
