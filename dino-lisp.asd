;;;; dino-lisp.asd

(asdf:defsystem #:dino-lisp
  :description "Dino lisp is a port of dinodeck engine for JRPG creationg"
  :author "Yeonho Jang <siabard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("sdl2" "sdl2-image" "sdl2-mixer" "sdl2-ttf" "cl-tiled")
  :components ((:module "engine"
		:serial t
		:components ((:file "renderer")
			     (:file "sprite")
			     (:file "texture")))
	       (:file "package")
               (:file "dino-lisp")))
