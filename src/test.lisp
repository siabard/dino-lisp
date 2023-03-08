(in-package #:dino-lisp)

(defun test/run-test (renderer font)
  (text/render-text renderer font *glyphs* "한산도 달 밝은 밤에 배에 수루에 홀로 앉아 어디서 일성호가는 남의 애를 끊나니" 10 10 100 100)
  (text/render-text renderer font *glyphs* "한산도" 10 80 50 100)
  (text/render-text renderer font *glyphs* "한겨레" 10 100 20 100))

(defun test/render-text ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "test"  :flags '(:shown) :w 800 :h 600)
      (sdl2-ttf:init)
      (init-global)
      (sdl2:with-renderer (renderer win)
	(let ((font-10 (sdl2-ttf:open-font (uiop:merge-pathnames* "assets/notokr-regular.ttf" *application-root*) 14)))
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sdl2:render-clear renderer)
		   (test/run-test renderer font-10)
		   (sdl2:render-present renderer))
	    (:quit ()
		   (text/destroy-font font-10)
		   (delete-global)
		   (sdl2-ttf:quit)
		   t)))))))

(defun test/gradient-test ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Gradient Box" :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(sdl2-image:init '(:jpg :png))
	(let* ((panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*))))

	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sdl2:render-clear renderer)

		   (sdl2:render-present renderer)
		   (sdl2:delay 40)
		   )
	    )
	  (sdl2:destroy-texture panel-texture)
	  )
	))))


(defparameter *ttt* nil)


;; TODO 유니코드 입력하다가 ascii 코드가 왔을 때 ascii 코드가 먼저오고 그 다음에
;; 유니코드가 온다?
;; 입력된 text 가 ascii면..
;; 1. 현재 mode가 unicode 면..
;;    1.1 일단 버퍼에 넣고.. ascii 로 전환한다.
;; 2. 현재 mode가 ascii 면..
;;    2.1  코드를 그대로 넣는다.
;; 입력된 text 가 unicode면..
;; 1. 현재 mode가 unicode 면..
;;    1.1 코드를 그대로 넣는다.
;; 2. 현재 mode 가 ascii 면..
;;
(defun test/text-input ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "text input" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let ((input-mode "")
	      (input-text nil)
	      (buffer nil))
	  (init-font "ascii"  "assets/ascii.png")
	  (init-font "hangul" "assets/hangul.png")
	  (sdl2-ffi.functions:sdl-start-text-input)
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text text)
			(format t "\"~A\"~%" text)
			(push text input-text))
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 255 255 255 255)
		   (sdl2:render-draw-rect renderer (sdl2:make-rect 96 96 158 88))
		   (when (< 0
			    (length input-text))
		     (draw-string renderer 104 104
				  (reverse (reduce (lambda (a b)
						     (concatenate 'string a b))
						   input-text))))
		   (sdl2:render-present renderer)
		   (sdl2:delay 40))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (delete-global-texture)
		   t)))))))


(defun test/dialog-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "text input" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let ((test-dialog (make-dialog-window 90 100 :title "테스트"
					       :texts '("이 글은 테스트입니다."
							"모쪼록 잘 나오길"))))
	  (init-font "ascii"  "assets/ascii.png")
	  (init-font "hangul" "assets/hangul.png")

	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 255 255 255 255)

		   (render-dialog-window test-dialog :renderer renderer)

		   (sdl2:render-present renderer)
		   (sdl2:delay 40))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (delete-global-texture)
		   t)))))))
