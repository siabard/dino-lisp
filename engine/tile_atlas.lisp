(in-package #:dino-lisp)

;;;; Texture 정보를 토대로 Atlas 정보를 만듦
;;;; texture 하나를 전체로 자르기 때문에
;;;; atlas의 특정 셀만을 써서 넣는 것을 추천
;;;; atlas는 source-rect 에 준하는 rect 형이므로
;;;; 적절한 셀만 넣는다면 큰 문제는 아님

(defun make-tile-atlas (texture tile-width tile-height &key (texture-height (sdl2:texture-height texture)) (texture-width (sdl2:texture-width texture)))
  (make-tile-atlas-raw texture-width texture-height tile-width tile-height))
  
(defun make-tile-atlas-raw (width height tile-width tile-height)
  (let ((tile-cols (floor width tile-width))
	(tile-rows (floor height tile-height)))
    (loop for y below tile-rows
	  nconcing (loop for x below tile-cols
			 collect (list-to-sdl2-rect (list (* x tile-width) (* y tile-height) tile-width tile-height))))))
