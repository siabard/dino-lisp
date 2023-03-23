(in-package #:dino-lisp)

;; dialog window
;; 이제는 그럼 texts 자체를 chunk로 만들어서 가지고 다니도록 하자.

(defclass dialog-window (gui) ((x :initarg :x
				  :accessor x)
			       (y :initarg :y
				  :accessor y)
			       (w :initarg :w
				  :accessor w)
			       (h :initarg :h
				  :accessor h)
			       (panel :initarg :panel
				      :accessor panel)
			       (index :initarg :index
				      :accessor index)
			       (title :initarg :title
				      :accessor title)
			       (texts-length :initarg :texts-length
					     :accessor texts-length)
			       (texts :initarg :texts
				      :accessor texts)
			       (avatar :initarg :avatar
				       :accessor avatar)
			       (choice :initarg :choice
				       :accessor choice)))

;;;; 생성자
;; cols, rows 가 0 이라면 각각 가능한 전체 크기에 들어갈 수 있는 크기를 사용한다.
;; (*logical-width* - x) / 16, (*logical-height* - y) / 16
;; 일단 화면의 최대 크기를 이용한다. (global의 값 이용)
;; cols ㅁㄴ
(defun make-dialog-window (x y &key (cols (/ (- *logical-width* x) 16)) (rows (/ (- *logical-height* y) 16)) title texts panel avatar choice)
  (let* ((chunked-texts (chunk-text texts cols rows))
	 (texts-length (length chunked-texts))
	 (first-para (car chunked-texts))
	 (full-length (apply #'max 0 (mapcar #'length first-para))))
    (make-instance 'dialog-window
		   :x x
		   :y y
		   :w (+ 16 (* full-length 16))
		   :h (cond ((eq nil title)
			     (+ 16 (* (length first-para) 16)))
			    (t (+ 36 (* (length first-para) 16))))
		   :index 0
		   :panel panel
		   :title title
		   :avatar avatar
		   :choice choice
		   :texts-length texts-length
		   :texts chunked-texts)))


;;; update gui

(defmethod update-gui ((gui dialog-window) dt)
  (panel/update (panel gui) dt))

;;; dialog 내용 바꾸기
(defgeneric set-dialog-window-texts (dialog-window texts w h)
  (:documentation "내용을 바꾸면서 w, h 도 같이 변경 "))


(defmethod set-dialog-window-texts (dialog-window texts words lines)
  (let* ((chunked-texts (chunk-text texts words lines))
	 (texts-length (length chunked-texts))
	 (full-length (apply #'max 0 (mapcar #'length (car chunked-texts))))
	 (avatar (avatar dialog-window))
	 (avatar-width (sdl2:rect-width (sprite-dest-rect avatar)))
	 (avatar-height (sdl2:rect-height (sprite-dest-rect avatar)))
	 (choice (choice dialog-window))
	 (title (title dialog-window)))
    (when (> (* 16 full-length) (- (w dialog-window) 16))
      (setf (w dialog-window) (+ 16 (* 16 full-length))))
    (when (> (* 16 (length texts)) (- (h dialog-window) 16))
      (setf (h dialog-window)
	    (cond ((eq title nil) (+ 16 (* 16 (length texts))))
		  (t (+ 36 (* 16 (length texts)))))))
    (when avatar
      (setf (w dialog-window)
	    (+ (w dialog-window)
	       avatar-width))
      (setf (h dialog-window)
	   (max (h dialog-window)
		avatar-height)))
    (when choice
      (setf (h dialog-window)
	    (+ (h dialog-window)
	       (render-height choice))))
    (setf (index dialog-window) 0)
    (setf (texts dialog-window) chunked-texts)
    (setf (texts-length dialog-window) texts-length)))

;;;; rendering 하기
(defgeneric render-dialog-window (dialog-window &key renderer)
  (:documentation "render dialog window"))

(defmethod render-dialog-window (dialog-window &key renderer)
  (let* ((x (+ 0 (x dialog-window)))
	 (y (+ 0 (y dialog-window)))
	 (w (+ 0 (w dialog-window)))
	 (h (+ 0 (h dialog-window)))
	 (title (title dialog-window))
	 (texts (texts dialog-window))
	 (current-texts (elt texts (index dialog-window)))
	 (avatar (avatar dialog-window))
	 (avatar-width (cond (avatar  (sdl2:rect-width (sprite-dest-rect avatar)))
			     (t 0)))
	 (panel (panel dialog-window))
	 (panel-tween-end (tween/end-p (panel-struct-tween (panel dialog-window)))))
    ;; 배경 패널 그리기
    (panel/render panel renderer x y w h)
    ;; 타이틀 있으면 타이틀 쓰기
    (when panel-tween-end
      (when title
	(draw-string renderer (+ x (cond ((eq nil (avatar dialog-window)) 8)
					 (t avatar-width)))
		     (+ y 8) title))
      ;; 아바타 출력하기
      (when avatar
	(sprite/render avatar renderer))
      ;; 내용 쓰기
      (dolist (text current-texts)
	(draw-string renderer
		     (+ x
			(cond ((eq nil (avatar dialog-window)) 8)
			      (t avatar-width)))
		     (+ y
			(cond ((eq nil title) 8)
			      (t 28)))
		     text)
	(incf y 16)))))

(defmethod update-gui ((gui dialog-window) dt)
  (panel/update (panel gui) dt))

(defmethod render-gui ((gui dialog-window) renderer)
  (render-dialog-window gui :renderer renderer))

;; 클릭시 처리
(defgeneric onclick-dialog-window (dialog-window callback)
  (:documentation "method for onclick"))

(defmethod onclick-dialog-window (dialog-window callback)
  (funcall callback dialog-window))
