;*******************************************************************************
;*
;*	File : Generate Conceptnames
;*
;*******************************************************************************

(defvar *cp-counter 0)
(defvar *cp-list nil)

(defun make-new-conceptname ()
  (let ((new (format nil "Concept~A" (incf *cp-counter)))
	)
       (push new *cp-list)
       new))

(defun clear-new-cp ()
  (setf *cp-counter 0)
  (setf *cp-list nil)
  )

(defun get-equal-concept (con)
  (dolist (cp *cp-list)
	  (if (conjunction-exists? con cp)
	      (return cp))))

(defun get-new-conceptname-of-number (number)
  (do ((list (reverse *cp-list) (cdr list))
       (counter (- number 1) (- counter 1))
       )
      ((= 0 counter) (car list))
      ))

(defun get-cnf-of-new-concept (number)
  (get-cnf-of-conceptname (get-new-conceptname-of-number number)))

