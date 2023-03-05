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
  description
  objectives
  effect)


;;; 효과들
;; progress-game : 게임의 진행사항을 바꿔준다.

;;; entity는 dialog 나 quest를 가질텐데
;;; 만약 인사를 하고 그 다음부터는 다른 대화를 원한다면..

;;; (make-dialog-item "tom-greet" "tom" nil "안녕" "tom-greet")
;;; (make-dialog-item "tom-again" "tom" '("tom-greet") "반갑소" nil)
;;; 의 두 항목을 dialog db 에 넣으면 된다.
;;; 이후 "tom"이라는 이름을 가지는 entity를 찾으면, "tom"에 대해 어떤
;;; 이벤트들을 진행했는지 검사하고, 그 결과를 가지고 dialog 아이템을 찾는다.
