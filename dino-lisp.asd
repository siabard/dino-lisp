;;;; dino-lisp.asd

(asdf:defsystem #:dino-lisp
  :description "Dino lisp is a port of dinodeck engine for JRPG creationg"
  :author "Yeonho Jang <siabard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("sdl2" "sdl2-image" "sdl2-ttf" "cl-tiled")
  :serial t
  :components ((:file "package")
               (:file "dino-lisp")	       
	       (:module "engine"
		:components ((:file "renderer")
			     (:file "sprite")
			     (:file "global")
			     (:file "tiled")
			     (:file "util")
			     (:file "tween")
			     (:file "map_tile")
			     (:file "tile_atlas")
			     (:file "texture")
			     (:file "entity")
			     (:file "key")
			     (:file "actions")
			     (:file "triggers")
			     (:file "mouse")
			     (:file "gui")))
	       (:module "src"
		:components ((:file "main")))))
