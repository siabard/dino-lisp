(in-package #:dino-lisp)

(defclass state-stack (gui)
  ((guis :initarg :guis
	 :accessor guis)
   (states :initarg :states
	   :accessor states)))


(defmethod update-gui ((gui state-stack) dt)
  (dolist (item (guis gui))
    (update-gui item dt))
  (dolist (state (states gui))
    (update-state state dt :keyboard nil :mouse nil)))


(defmethod handle-input-gui ((gui state-stack) &key keyboard mouse)
  (dolist (state (states gui))
    (handle-input-state state :keyboard keyboard :mouse mouse)))

(defmethod render-gui ((gui state-stack) renderer)
  (dolist (item (guis gui))
    (render-gui item renderer)))


(defgeneric add-fixed (state-stack x y text &key panel w h)
  (:documentation "Add fixed text"))

(defmethod add-fixed ((gui state-stack) x y text &key panel (w (- *logical-width* x)) (h (- *logical-height* y)))
  (let* ((dialog (make-dialog-window x y :panel panel :texts (list text) :cols (floor w 16) :rows (floor h 16))))
    (push dialog (guis gui))))


(defun make-state-stack (&key guis states)
  (make-instance 'state-stack :guis guis :states states))
