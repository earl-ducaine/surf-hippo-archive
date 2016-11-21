;*******************************************************************************
;*
;*	Choice-Points
;*
;*******************************************************************************

;;; compiler instructions

(proclaim '(inline make-choice-point choice-point? first-alternative rest-alternatives))

;;; representation of choicepoints

(defun make-choice-point (alternatives)
  alternatives)

(defun choice-point? (trail-entry)
  (not (functionp trail-entry)))  ;;; other trail-entries are functions

(defun first-alternative (alternatives)
  (car alternatives))
(defun rest-alternatives (alternatives)
  (cdr alternatives))
(defun still-alternatives? (alternatives)
  (cadr alternatives))

(defun make-push-disjunction-on-or-function! (disj)
  #'(lambda () (push-concepts-on-or! disj)))
