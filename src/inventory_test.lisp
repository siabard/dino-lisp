(in-package #:dino-lisp)

(defun inventory-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Inventory"
			   :flags '(:shown)
			   :w 800
			   :h 600)
      (sdl2:with-renderer (renderer win
				    :flags '(:accelerated :targettexture :presentvsync))
	(setf *renderer* renderer)
	(sdl2-image:init '(:jpg :png))
	(let* ((item-texture (load-texture renderer (uiop:merge-pathnames*
						     "assets/items.png"
						     *application-root*)))
	       (item-atlas (make-tile-atlas item-texture 16 16)))
	  (create-item *item-db*
		       :category "weapon"
		       :name "short sword"
		       :texture item-texture
		       :atlas (elt item-atlas 1))
	  (create-item *item-db*
		       :category "shield"
		       :name "wooden shield"
		       :texture item-texture
		       :atlas (elt item-atlas 3))
	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (sdl2:render-clear renderer)
		   (maphash (lambda (k v)
			      (render-item v renderer 50 50)) *item-db*)
		   (sdl2:render-present renderer)
		   (sdl2:delay 8))
	    (:quit ()
		   (delete-global)
		   (clrhash *item-db*))))))))
