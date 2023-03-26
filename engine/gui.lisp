(in-package #:dino-lisp)

;; gui
(defclass gui () ())

(defgeneric update-gui (gui dt)
  (:documentation "update gui with when elapsed dt"))

(defgeneric render-gui (gui renderer)
  (:documentation "rendering gui"))

(defgeneric render-width (gui)
  (:documentation "rendering width"))

(defgeneric render-height (gui)
  (:documentation "rendering height"))

(defgeneric set-pos-gui (gui new-x new-y)
  (:documentation "set x, y position"))

;; 키 입력처리
(defgeneric process-key-event (gui scancode)
  (:documentation "key event 처리"))

(defmethod set-pos-gui (gui new-x new-y)
  (setf (x gui) new-x)
  (setf (y gui) new-y))


;; 입력 (키, 마우스 처리)

(defgeneric handle-input-gui (gui &key keyboard mouse)
  (:documentation "키보드 / 마우스 처리"))
