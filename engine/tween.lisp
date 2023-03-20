(in-package #:dino-lisp)

(defstruct tween start end timespan current-time running)

;; t : 0~1
(defun tween/linear (tw)
  (let* ((start (tween-start tw))
	 (end   (tween-end tw))
	 (ct    (tween-current-time tw))
	 (ts    (tween-timespan tw))
	 (ratio (cond ((= 0 ts) 1)
		      (t  (/ ct ts))))
	 (dt    (cond ((>= ratio 1) 1)
		      (t ratio))))
    (cond ((tween-running tw)
	   (floor (+ (* (- 1  dt) start)
		     (* dt end))))
	  (t end))))


(defun tween/easy-in-circle (tw)
  (let* ((start (tween-start tw))
	 (end  (tween-end   tw))
	 (change (- end start))
	 (ct    (tween-current-time tw))
	 (ts    (tween-timespan tw))
	 (ratio (cond ((= 0 ts) 0)
		      ((>= ct ts) 1)
		      (t  (/ ct ts))))
	 (dt    (cond ((>= ratio 1) 1)
		      (t ratio))))
    (cond ((tween-running tw)
	   (+ start (* change (- 1 (sqrt (- 1 (* dt dt)))))))
	  (t end))))

;; tween 종료?
(defun tween/end-p (tw)
  (let ((current-time (tween-current-time tw))
	(timespan (tween-timespan tw)))
    (>=  current-time timespan)))


;; tween 종료
(defun tween/stop (tw)
  (setf (tween-start    tw) 0)
  (setf (tween-running  tw) nil))

;; time 증가
;; 종료확인하여 종료되면 tween 멈춤
(defun tween/update-dt (tw dt)
  (incf (tween-current-time tw) dt)
  (when (tween/end-p tw)
    (tween/stop tw)))
