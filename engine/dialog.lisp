(in-package #:dino-lisp)


;;; 각각의 다이얼로그는 말을 하는 대상과 조건, 그 이후의 효과를 가진다.

(defstruct dialog-item
  name
  who
  condition
  texts
  effect)


;;; 각각의 퀘스트는 퀘스트 명과 설명, 퀘스트의 조건들, 그리고 효과를 가진다.
(defstruct quest-item
  name
  giver
  answerer
  condition
  description
  objectives
  reward
  effect
  done)


;;; 효과들

;;; entity는 dialog 나 quest를 가질텐데
;;; 만약 인사를 하고 그 다음부터는 다른 대화를 원한다면..

;;; (make-dialog-item "tom-greet" "tom" nil "안녕" "tom-greet")
;;; (make-dialog-item "tom-again" "tom" '("tom-greet") "반갑소" nil)
;;; 의 두 항목을 dialog db 에 넣으면 된다.
;;; 이후 "tom"이라는 이름을 가지는 entity를 찾으면, "tom"에 대해 어떤
;;; 이벤트들을 진행했는지 검사하고, 그 결과를 가지고 dialog 아이템을 찾는다.


;;; 퀘스트는 주는 사람과 보상을 주는 사람이 각기 다를 수 있다.
;;; 그래서 giver와 answerer가 따로 있다.
;;; (make-quest-item "tom-goto" nil "tom" "tom에게 인사" '("tom-greet") nil nil nil)
;;; 처럼하면 tom에게 가서 인사하는 퀘스트를 만들 수 있다.
;;; "tom-greet" 은 game-progress에 등록된다.
;;; quest는 완료되면 done 을 T로 설정한다.

(defun create-quest (quest-db name &key description objectives (giver nil) (answerer nil) (condition nil) (reward nil) (effect nil) (done nil))
  (let ((quest (make-quest-item :name name
				:description description
				:objectives objectives
				:giver giver
				:answerer answerer
				:condition condition
				:reward reward
				:effect effect
				:done done)))
    (setf (gethash name quest-db) quest)))

;;; quest-log 에 quest 를 등록
(defun give-quest (quest-db name quest-log)
  (setf (gethash name quest-log) (gethash name quest-db)))


;;; quest 일부를 완료시키기
(defun complete-quest (game-progress quest-log name item)
  (let* ((quest (gethash name quest-log))
	 (objectives (quest-item-objectives quest))
	 (new-objectives (remove-if (lambda (obj) (equal obj item)) objectives)))
    ;; objective 줄이기
    (setf (quest-item-objectives quest) new-objectives)
    (when (not new-objectives )
      ;; 모든 일을 다 했다면 quest 를 완료로 한다.
      (setf (quest-item-done quest) T)
      (remhash name quest-log)
      (push name game-progress))))



;;; 대화시스템
(defun create-dialog (dialog-db name &key who condition texts effect)
  (let ((dialog (make-dialog-item :name name
				  :who who
				  :condition condition
				  :texts texts
				  :effect effect)))
    (setf (gethash name dialog-db) dialog)))
