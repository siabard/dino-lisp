(in-package #:dino-lisp)

(defun game-main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Dinodeck" :flags '(:shown) :w 800 :h 600)
      (sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	(sdl2:with-event-loop (:method :poll)
	  (:quit ()
		 t))))))
