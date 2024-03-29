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
	(sdl2-ttf:init)
	(let* ((font-10 (sdl2-ttf:open-font (uiop:merge-pathnames* "assets/notokr-regular.ttf" *application-root*) 14))
	       (panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
	       (text-panel (make-textbox :text '("테스트")
					 :font font-10
					 :textpanel (panel/setup panel-texture 3 3)))
	       (current-time (sdl2:get-ticks)))
	  (textbox/set-text text-panel renderer "테스트")
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (let* ((dt (- (sdl2:get-ticks) current-time)))
		     (sdl2:render-clear renderer)

		     (textbox/update text-panel dt)
		     (textbox/render text-panel renderer 20 20 150 150)
		     (sdl2:render-present renderer)
		     (setf current-time (sdl2:get-ticks))
		     (sdl2:delay 40)))
	    (:quit ()
		   t))
	  (sdl2-ttf:quit)
	  (sdl2:destroy-texture panel-texture)
	  )))))


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
	  (sdl2-ffi.functions:sdl-set-text-input-rect (sdl2:make-rect 100 100 150 80))
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text text)
			(format t "~A~%" (mapcar #'char-code  (coerce text 'list)))
			(format t "INPUT ~A~%" text)
			(push text input-text))
	    (:textediting (:text text :start start :length length)
			  (format t "~A~%" (mapcar #'char-code  (coerce text 'list)))
			  (format t "EDITING ~A ~A ~A ~%" text start length))
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 255 255 255 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-draw-rect renderer (sdl2:make-rect 96 96 158 88))
		   (when (< 0
			    (length input-text))
		     (draw-string renderer 104 104
				  (reverse (reduce (lambda (a b)
						     (concatenate 'string a b))
						   input-text))))
		   (sdl2:render-present renderer))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (delete-global-texture)
		   t)))))))


(defun test/dialog-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "text input" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((long-texts '("이 글은 정말 긴 글입니다. 그래서 중간이 잘려야하죠."
			     "물론 이런 정책이 늘 있는 것은 아니지만 캐릭터간의 대화는 당연히 잘려야하지 않을가요?"))
	       (panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
	       (char-texture (load-texture renderer (uiop:merge-pathnames* "assets/mychar.png" *application-root*)))
	       (label-1 (make-label 0 0 "레이블 1"))
	       (label-2 (make-label 0 0 "레이블 2"))
	       (cursor-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
	       (test-dialog (make-dialog-window 90 100
						:cols 12 :rows 4
						:title "테스트"
						:panel (panel/setup panel-texture 3 3)
						:avatar (make-sprite :texture char-texture
								     :source-rect (sdl2:make-rect 0 0 16 16)
								     :dest-rect (sdl2:make-rect 94 104 16 16))
						:texts long-texts))
	       (test-dialog2 (make-dialog-window 90 300
						 :cols 12 :rows 4
						 :title "테스트"
						 :panel (panel/setup panel-texture 3 3)
						 :avatar (make-sprite :texture char-texture
								      :source-rect (sdl2:make-rect 0 0 16 16)
								      :dest-rect (sdl2:make-rect 94 104 16 16))
						 :texts long-texts))
	       (test-dialog3 (make-dialog-window 290 180
						 :cols 12 :rows 4
						 :title "테스트"
						 :panel (panel/setup panel-texture 3 3)
						 :avatar (make-sprite :texture char-texture
								      :source-rect (sdl2:make-rect 0 0 16 16)
								      :dest-rect (sdl2:make-rect 94 104 16 16))
						 :texts long-texts
						 :choice (make-choice-dialog 50 50
									     :datasource (list label-1 label-2)
									     :item-width 80
									     :item-height 16
									     :cursor (make-sprite :texture cursor-texture
												  :source-rect (sdl2:make-rect 0 0 9 9)
												  :dest-rect (sdl2:make-rect 0 0 9 9))
									     :columns 2
									     :display-start 0
									     :display-rows 2)))
	       (current-time (sdl2:get-ticks))
	       (keyboard (make-instance 'key-input))
	       (mouse    (make-mouse-system)))
	  (init-font "ascii"  (uiop:merge-pathnames* "assets/ascii.png" *application-root*))
	  (init-font "hangul" (uiop:merge-pathnames* "assets/hangul.png" *application-root*))
	  (init-keys keyboard)

	  (sdl2:with-event-loop (:method :poll)
	    (:mousebuttondown ()
			      (onclick-dialog-window test-dialog
						     (lambda (dialog)
						       (when (< (index dialog)
								(- (length (texts dialog)) 1))
							 (incf (index dialog))))))
	    (:keydown (:keysym keysym)
		      (keydown-event keyboard (sdl2:scancode-value keysym)))
	    (:keyup (:keysym keysym)
		    (keyup-event keyboard (sdl2:scancode-value keysym)))
	    (:idle ()
		   (let* ((dt (- (sdl2:get-ticks) current-time)))
		     (sdl2:set-render-draw-color renderer 0 0 0 255)
		     (sdl2:render-clear renderer)
		     (sdl2:set-render-draw-color renderer 255 255 255 255)

		     ;;(update-gui test-dialog dt)
		     ;;(update-gui test-dialog2 dt)
		     (update-gui test-dialog3 dt)

		     (handle-input-gui test-dialog3 :keyboard keyboard :mouse mouse)
		     ;;(render-dialog-window test-dialog :renderer renderer)
		     ;;(render-dialog-window test-dialog2 :renderer renderer)
		     (render-dialog-window test-dialog3 :renderer renderer)

		     (sdl2:render-present renderer)
		     (setf current-time (sdl2:get-ticks))
		     (clear-keys keyboard)
		     (sdl2:delay 25)))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (sdl2:destroy-texture panel-texture)
		   (sdl2:destroy-texture char-texture)
		   (sdl2:destroy-texture cursor-texture)
		   (delete-global-texture)
		   t)))))))


(defun test/choice-select ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "text input" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((label-1 (make-label 0 0 "레이블 1"))
	       (label-2 (make-label 0 0 "레이블 2"))
	       (label-3 (make-label 0 0 "레이블 3"))
	       (label-4 (make-label 0 0 "레이블 4"))
	       (label-5 (make-label 0 0 "레이블 5"))
	       (label-6 (make-label 0 0 "레이블 6"))
	       (label-7 (make-label 0 0 "레이블 7"))
	       ;;(label-8 (make-label 0 0 "레이블 8"))
	       (cursor-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
	       (dialog (make-choice-dialog 50 50
					   :datasource (list label-1 label-2 label-3 label-4 label-5 label-6 label-7 )
					   :item-width 80
					   :item-height 16
					   :cursor (make-sprite :texture cursor-texture
								:source-rect (sdl2:make-rect 0 0 9 9)
								:dest-rect (sdl2:make-rect 0 0 9 9))
					   :columns 2
					   :display-start 0
					   :display-rows 2))
	       (keyboard (make-instance 'key-input))
	       (mouse    (make-mouse-system)))
	  (init-font "ascii"  "assets/ascii.png")
	  (init-font "hangul" "assets/hangul.png")
	  (init-keys keyboard)
	  (sdl2:with-event-loop (:method :poll)
	    (:mousebuttonup (:button button)
			    (cond ((= button 1)
				   (setf (mouse-system-button-l mouse) nil))
				  ((= button 3)
				   (setf (mouse-system-button-r mouse) nil))))
	    (:mousebuttondown (:button button)
			      (cond ((= button 1)
				     (setf (mouse-system-button-l mouse) nil))
				    ((= button 3)
				     (setf (mouse-system-button-r mouse) nil))))
	    (:keydown (:keysym keysym)
		      (keydown-event keyboard (sdl2:scancode-value keysym)))
	    (:keyup (:keysym keysym)
		    (keyup-event keyboard (sdl2:scancode-value keysym)))
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 255 255 255 255)
		   (handle-input-gui dialog :mouse mouse :keyboard keyboard)

		   (render-gui dialog renderer)


		   (sdl2:render-present renderer)
		   (clear-keys keyboard)
		   (sdl2:delay 40))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (delete-global-texture)
		   t)))))))




(defun bitmap-test-main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "bitmap font" :flags '(:shown) :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(init-font "ascii"  "assets/ascii.png")
	(init-font "hangul" "assets/hangul.png")
	(let ((textbox (make-dialog-window 40
					   60
					   :cols 12
					   :rows 4
					   :title "제목"
					   :texts '("Hello World"
						    "안녕하세요."
						    "숫자 1234 number"))))
	  (sdl2:with-event-loop (:method :poll)
	    (:quit ()
		   (format t "END")
		   t)
	    (:idle ()

		   (sdl2:set-render-draw-color renderer 0 0 0 255 )
		   (sdl2:render-clear renderer)

		   (render-dialog-window textbox :renderer renderer)

		   (sdl2:render-present renderer)
		   (sdl2:delay 16)))))
      (delete-global-texture))))


(defun test/progress-bar ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "progress-bar" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((bar-texture (load-texture renderer (uiop:merge-pathnames* "assets/progress_bar.png" *application-root*)))
	       (hpbar (make-progress-bar bar-texture
					 :x 100
					 :y 100
					 :w 80
					 :h 20
					 :atlas (make-tile-atlas bar-texture 2 2)
					 :value 20
					 :max-value 100))

	       )
	  (sdl2:with-event-loop (:method :poll)
	    (:mousebuttondown ())
	    (:keydown ())
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 255 255 255 255)

		   (render-gui hpbar renderer)


		   (sdl2:render-present renderer)
		   (sdl2:delay 40))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (sdl2:destroy-texture bar-texture)
		   (delete-global-texture)
		   t)))))))


(defun test/scroll-bar ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "scroll-bar" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((bar-texture (load-texture renderer (uiop:merge-pathnames* "assets/scrollbar.png" *application-root*)))
	       (hpbar (make-scroll-bar bar-texture
				       :x 100
				       :y 100
				       :w 18
				       :h 120
				       :atlas  (make-tile-atlas bar-texture 18 18)
				       :value 20
				       :max-value 100))

	       )
	  (sdl2:with-event-loop (:method :poll)
	    (:mousebuttondown ())
	    (:keydown ())
	    (:idle ()
		   (sdl2:set-render-draw-color renderer 0 0 0 255)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 255 255 255 255)

		   (render-gui hpbar renderer)


		   (sdl2:render-present renderer)
		   (sdl2:delay 40))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (sdl2:destroy-texture bar-texture)
		   (delete-global-texture)
		   t)))))))

(defun test/stack-state ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "scroll-bar" :w 640 :h 480)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(let* ((panel-texture (load-texture renderer (uiop:merge-pathnames* "assets/panel.png" *application-root*)))
	       (panel1 (panel/setup panel-texture 3 3))
	       (panel2 (panel/setup panel-texture 3 3))
	       (panel3 (panel/setup panel-texture 3 3))
	       (state_stack (make-state-stack))
	       (current-time (sdl2:get-ticks)))
	  (init-font "ascii"  (uiop:merge-pathnames* "assets/ascii.png" *application-root*))
	  (init-font "hangul" (uiop:merge-pathnames* "assets/hangul.png" *application-root*))
	  (add-fixed state_stack 50 60 "안녕하세요" :w 200 :h 150 :panel panel1)
	  (add-fixed state_stack 55 65 "반가와요" :w 200 :h 150 :panel panel2)
	  (add-fixed state_stack 60 70 "잘부탁해요" :w 200 :h 150 :panel panel3)
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (let* ((dt (- (sdl2:get-ticks) current-time)))

		     (sdl2:set-render-draw-color renderer 0 0 0 255)
		     (sdl2:render-clear renderer)
		     (sdl2:set-render-draw-color renderer 255 255 255 255)

		     (update-gui state_stack dt)
		     (render-gui state_stack renderer)

		     (sdl2:render-present renderer)

		     (setf current-time (sdl2:get-ticks))
		     (sdl2:delay 25)
		     ))
	    (:quit ()
		   (sdl2-ffi.functions:sdl-stop-text-input)
		   (sdl2:destroy-texture panel-texture)
		   (delete-global-texture)
		   t)))))))
