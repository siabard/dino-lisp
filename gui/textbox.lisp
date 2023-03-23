(in-package #:dino-lisp)

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
