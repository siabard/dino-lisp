(in-package #:dino-lisp)

;;;; tiled test-sdl2

;; load tilemap
(defun load-tiled-map (path-to-map-file)
  "Return tiled map structure. Return NIL when loading is failed"
  (let ((map-path (uiop:merge-pathnames*  path-to-map-file *application-root*)))
    (when (probe-file map-path)
      (cl-tiled:load-map map-path))))

