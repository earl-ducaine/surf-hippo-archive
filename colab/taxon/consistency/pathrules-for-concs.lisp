;************************************************************
;*                                                          *
;*                File : Pathrules for Concepts
;*                                                          *
;************************************************************


(defun FAC (attr next-function obj)
  (print-rule 'FAC attr obj)
  (cond ((unknown? obj)
	 (add-printable-function! obj #'print-cond-fct-of-F_C next-function attr)
	 (add-abstract-function! (make-FAC attr next-function) obj))
	((concrete? obj))
	(t (let ((value (get-attr-filler attr obj))
		 )
		(cond (value
		       (funcall next-function value))
		      (t (add-cond-funcs-erasable! obj (make-attr-cf-C attr next-function))
			 (add-printable-cond-funcs-erasable! obj #'print-cond-fct-of-F_C 
							     next-function attr)
			 ))))))

(defun FRC (role next-function obj)
  (print-rule 'FRC role obj)
  (cond ((unknown? obj)
	 (add-printable-function! obj #'print-cond-fct-of-F_C next-function role)
	 (add-abstract-function! (make-FRC role next-function) obj))
	((concrete? obj))
	(t (add-cond-funcs-erasable! obj 
				     (make-role-cf-C role next-function))
	   (add-printable-cond-funcs-erasable! obj #'print-cond-fct-of-F_C next-function role)
	   (dolist (value (get-all-role-fillers role obj))
		   (funcall next-function value))
	   )))

(defun SAC (attr next-function obj)
  (print-rule 'SAC attr obj)
  (cond ((unknown? obj)
	 (let ((new-obj (make-new-obj))
	       )
	      (attr-rule obj attr new-obj)
	      (funcall next-function (deref new-obj))))
	((concrete? obj)
	 (clash 'SAC 'New obj))
	(t (let ((value (get-attr-filler attr obj))
		 )
		(cond (value
		       (funcall next-function value))
		      (t (let ((new-obj (make-new-obj))
			       )
			      (attr-rule obj attr new-obj)
			      (funcall next-function (deref new-obj))
			      ))
		      )))
	))

(defun SRC (role next-function obj)
  (print-rule 'SRC role obj)
  (let ((new-obj (make-new-obj))
	)
       (role-rule obj role new-obj)
       (funcall next-function (deref new-obj))
       ))

(defun make-FAC (attr next-function)
  #'(lambda (obj) (FAC attr next-function (deref obj))))
(defun make-FRC (role next-function)
  #'(lambda (obj) (FRC role next-function (deref obj))))

