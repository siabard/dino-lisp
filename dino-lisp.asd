;;;; dino-lisp.asd

(asdf:defsystem #:dino-lisp
  :description "Dino lisp is a port of dinodeck engine for JRPG creationg"
  :author "Yeonho Jang <siabard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("swank" "sdl2" "sdl2-image" "sdl2-ttf" "cl-tiled" "imago")
  :serial t
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "dino-lisp"
  :entry-point "dino-lisp::game_main"
  :components ((:file "package")
               (:file "dino-lisp")	       
	       (:module "engine"
		:components ((:file "renderer")
			     (:file "sprite")
			     (:file "global")
			     (:file "dialog")
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
			     (:file "gui")
			     (:file "camera")
			     (:file "hangul")
			     (:file "scene")
			     (:file "ecs")
			     (:file "component")
			     (:file "bitmap_font")
			     (:file "text")))
	       (:module "src"
		:components ((:file "test")
			     (:file "main")
			     (:file "sinbi_city")
			     (:file "inventory_test")))
	       (:module "util"
		:components ((:file "swank")
			     (:file "uid")))
	       (:module "system"
		:components ((:file "inventory")))
	       (:module "states"
		:components ((:file "state")
			     (:file "explore_state")))
	       (:module "gui"
		:components ((:file "progress_bar")
			     (:file "scroll_bar")
			     (:file "label")
			     (:file "textbox")
			     (:file "panel")
			     (:file "dialog_window")
			     (:file "choice_dialog")
			     (:file "state_stack")))))
