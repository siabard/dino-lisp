(in-package #:dino-lisp)

(defun test/run-test (renderer font)
  (text/render-char renderer font *glyphs* "한산도" 10 10)
  (text/render-char renderer font *glyphs* "한산도" 10 80)
  (text/render-char renderer font *glyphs* "한겨레" 10 100))

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
	    (:quit () t)))))))
