;;;; dino-lisp.asd

(asdf:defsystem #:dino-lisp
  :description "Dino lisp is a port of dinodeck engine for JRPG creationg"
  :author "Yeonho Jang <siabard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("sdl2" "sdl2-image" "sdl2-mixer" "sdl2-ttf" "cl-tiled")
  :serial t
  :components ((:file "package")
               (:file "dino-lisp")	       
	       (:module "engine"
		:components ((:file "renderer")
			     (:file "sprite")
			     (:file "texture")))
	       (:module "src"
		:components ((:file "main")))))
