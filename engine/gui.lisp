(in-package #:dino-lisp)

;; panel

;; panel은 9개의 tile로 이루어진 집합으로
;; 네 모서리(Top Left, Top Right, Bottom Left, Bottom Right)는 1:1로 출력되고
;; 네 변은 Panel의 가로/세로에서 tile의 가로/세로를 두 배한 만큼 뺀 크기를 가진다.
;; 해당 TILE을 노출할 때 src_rect -> dest_rect 를 변환하는 작업을 거치면 된다.

(defstruct panel-struct texture atlas atlas-width atlas-height)

(defun panel/setup (texture width height)
  (let* ((panel (make-panel-struct :texture texture :atlas-width width :atlas-height height))
	 (atlas (make-tile-atlas texture width height)))
    (setf (panel-struct-atlas panel) atlas)
    panel))


(defun panel/draw-partial (renderer texture source-rect dest-rect)
  (let ((sprite (make-sprite :texture texture
			     :source-rect source-rect
			     :dest-rect dest-rect)))
    (sprite/render sprite renderer)))

(defun panel/render (panel renderer x y w h)
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
    (panel/draw-partial renderer panel-texture panel-top-left (sdl2:make-rect x y panel-width panel-height))
    (panel/draw-partial renderer panel-texture panel-top-mid  (sdl2:make-rect (+  x panel-width) y panel-width-span panel-height))
    (panel/draw-partial renderer panel-texture panel-top-right (sdl2:make-rect (+  x panel-width panel-width-span) y panel-width panel-height))
    (panel/draw-partial renderer panel-texture panel-mid-left (sdl2:make-rect x (+  y panel-height) panel-width  panel-height-span))
    (panel/draw-partial renderer panel-texture panel-mid-mid  (sdl2:make-rect (+  x panel-width) (+  y panel-height) panel-width-span panel-height-span))
    (panel/draw-partial renderer panel-texture panel-mid-right (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height) panel-width panel-height-span))
    (panel/draw-partial renderer panel-texture panel-bot-left (sdl2:make-rect x (+  y panel-height panel-height-span) panel-width panel-height))
    (panel/draw-partial renderer panel-texture panel-bot-mid  (sdl2:make-rect (+  x panel-width)  (+  y panel-height panel-height-span) panel-width-span panel-height))
    (panel/draw-partial renderer panel-texture panel-bot-right (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height panel-height-span) panel-width panel-height))))
