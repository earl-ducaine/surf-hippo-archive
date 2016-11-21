;*******************************************************************************
;*
;*	Stacks
;*
;*******************************************************************************

;;; Compiler Instructions

(proclaim '(inline empty-main-stack? empty-or-stack? 
		   pop-main push-on-main!
		   pop-or push-concepts-on-or! get-or reset-or!
		   trail! 
		   ))

(defvar *main nil)
(defvar *or nil)
(defvar *trail nil)
(defvar *trail-copy nil)

(defvar *copy-trail? nil)

(defun clear-main ()
  (setq *main nil)
  )
(defun clear-or ()
  (setq *or nil)
  )
(defun clear-trail ()
  (setf *trail nil)
  )
(defun clear-stacks ()
  (clear-or)
  (clear-main)
  (clear-trail)
  )

(defun empty-main-stack? ()
  (null *main))
(defun empty-or-stack? ()
  (null *or))

(defun pop-main ()
  (pop *main)
  )
(defun pop-or ()
  (pop *or)
  )
(defun get-or ()
  *or)
(defun reset-or! (old)
  #'(lambda () (setf *or old)))

(defun push-on-main! (function)
  (push function *main)
  )
(defun push-concepts-on-or! (concepts)
  (push concepts *or)
  )
(defun trail! (function)
  (push function *trail)
  )

(defun copy-trail! (function)
  (if *copy-trail?
      (push function *trail-copy)
      ))
