(in-package #:dino-lisp)

;;;; porting from https://github.com/Samuel85/SDL_DrawText.git
;;;; original code is c++

(defun text/render-text (renderer font glyphs char x y width height)
  (let ((target-x x)
	(target-y y)
	(text-boundary-right  (+ x width))
	(text-boundary-bottom (+ y height)))
    (loop for idx from 0 to (- (length char) 1)
	  do (let* ((ch (string (aref char idx)))
		    (org-glyph (gethash ch glyphs))
		    (glyph (if (not  org-glyph)
			       (let* ((surface (sdl2-ttf:render-utf8-solid font ch 255 255 255 0))
				      (texture (sdl2:create-texture-from-surface renderer surface)))

				 (setf (gethash ch glyphs) texture)
				 texture)
			       org-glyph))
		    (glyph-width  (sdl2:texture-width  glyph))
		    (glyph-height (sdl2:texture-height glyph))
		    (w (if (>= text-boundary-right (+ target-x glyph-width))
			   glyph-width
			   (- text-boundary-right target-x)))
		    (h (if (>= text-boundary-bottom (+ target-y glyph-height))
			   glyph-height
			   (- text-boundary-bottom target-y))))
	       (when (> h 0)
		 (when (<= w 0)
	      	   (setf target-x x)
		   (incf target-y glyph-height))
	         (sdl2:render-copy-ex renderer
				      glyph
				      :source-rect (sdl2:make-rect 0 0 glyph-width glyph-height)
				      :dest-rect (sdl2:make-rect target-x target-y glyph-width glyph-height)))
	       (incf target-x glyph-width)))))


(defun text/destroy-font (font)
  (safe-close-font font))


(defun split-list (list len)
 ;; (split-list '(a b c d e f g) 3) => ((A B C) (D E F) (G))
 "Splits the list into sublists of length len. The last element might have fewer than len elements."
    (do* ((n 1 (1+ n))
          (l list (cdr l))
          (l1 nil)
          (res nil) )
         ((null l) (progn (when l1 (push (nreverse l1) res))(nreverse   res)))
        (push (car l) l1)
        (when (= n len)
            (push (nreverse l1) res)
            (setq l1 nil)
            (setq n 0) )))


;; 텍스트(스트링의 배열)을 일정 크기의 가로/세로에 맞는 크기로
;; 자른 텍스트 뭉치 배열로 만들기

(defun chunk-text (texts width height)
  (let ((chunk '()))
    (dolist (text texts)
      (push (mapcar (lambda (lst) (string-trim " " (coerce lst 'string)))
		    (split-list (coerce text 'list) width))
	    chunk))
    (split-list  (reduce #'append  (reverse  chunk)) height)))
