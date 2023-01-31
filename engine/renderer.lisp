;;;; engine/renderer.lisp

(in-package #:dino-lisp)

;; 실제 화면 크기와 출력되는 가상 화면 크기
;; 그리고, 가상 화면 크기에 특정 영역을 제약하는
;; 카메라 크기를 통해서 화면에 출력해야한다.

;; 실제 화면 크기를 real_rect, 가상 화면 크기를 virtual_rect
;; 카메라 크기를 camera_rect 라고 할 때
;; 가상 화면을 출력할 카메라 영역을 설정하고,
;; 카메라 영역을 실제 화면 크기로 매핑한다.

(defstruct render_rect x y w h)

;; pointer 로 만들어진 공간 (foreign-alloc 등으로 생성) 에
;; 지정된 좌표 (x, y)와 너비(w) 의 메모리 위치에
;; RGBA 값 (4바이트 - 32비트) 색상을 그린다.
(defun pointer/draw-pixel-rgba  (pixels x y w c)
  (let ((pos (+ x (* y w))))
    (setf (cffi:mem-ref pixels :uint32 pos) c)))


;; 아래와 같은 의사코드로 모든 state는 그린다.
;; dest-rect 는 camera-rect 의 위치를 가져야한다.

(defun state-render (renderer target-texture)
  (sdl2:set-render-draw-blend-mode sdl2-ffi:+sdl-blendmode-blend+)
  (let ((texture (sdl2:create-texture renderer sdl2:+pixelformat-rgba8888+ sdl2-ffi:+sdl-textureaccess-target+ 320 240)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (sdl2:set-render-target renderer texture)
    ;; draw somthing on texture

    (sdl2:set-render-target renderer target-texture)
    (sdl2:render-copy-ex renderer textture
			 :source-rect (sdl2:make-rect 0 0 320 240)
			 :dest-rect (sdl2:make-rect 0 0 320 240)
			 :angle 0
			 :center (sdl2:make-point 0 0)
			 :flip nil)
    (sdl2:destroy-texture texture)))
