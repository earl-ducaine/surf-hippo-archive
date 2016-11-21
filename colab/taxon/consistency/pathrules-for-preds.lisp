;************************************************************
;*                                                          *
;*                File : pathrules
;*                                                          *
;************************************************************


(defun FAP (attr next-function pred first obj)
  (print-rule 'FAP attr obj)
  (cond ((unknown? obj)
	 (add-printable-function! obj #'print-cond-fct-of-F_P 
					   next-function attr first pred)
	 (add-abstract-function! (make-FAP attr next-function pred first) obj))
	((concrete? obj))
	(t (let ((value (get-attr-filler attr obj))
		 )
		(cond (value
		       (funcall next-function pred first value))
		      (t (add-printable-cond-funcs-erasable! obj #'print-cond-fct-of-F_P
							     next-function attr first pred)
			 (add-cond-funcs-erasable! obj 
						   (make-attr-cond-function attr 
									    next-function
									    pred
									    first)))
		      )))))

(defun FRP (role next-function pred first obj)
  (print-rule 'FRP role obj)
  (cond ((unknown? obj)
	 (add-printable-function! obj #'print-cond-fct-of-F_P 
					   next-function role first pred)
	 (add-abstract-function! (make-FRP role next-function pred first) obj))
	((concrete? obj))
	(t (add-cond-funcs-erasable! obj 
				     (make-role-cond-function role next-function pred first))
	   (add-printable-cond-funcs-erasable! obj #'print-cond-fct-of-F_P 
					       next-function role first pred)
	   (dolist (value (get-all-role-fillers role obj))
		   (funcall next-function pred first value))
	   )))

(defun SAP (attr next-function pred first obj)
  (print-rule 'SAP attr obj)
  (cond ((unknown? obj)
	 (let ((new-obj (make-new-obj))
	       )
	      (attr-rule obj attr new-obj)
	      (funcall next-function pred first new-obj)))
	((concrete? obj)
	 (clash 'SAP 'New obj))
	(t (let ((value (get-attr-filler attr obj))
		 )
		(cond (value
		       (funcall next-function pred first value))
		      (t (let ((new-obj (make-new-obj))
			       )
			      (attr-rule obj attr new-obj)
			      (funcall next-function pred first new-obj)
			      ))
		      )))
	))

(defun SRP (role next-function pred first obj)
  (print-rule 'SRP role obj)
  (let ((new-obj (make-new-obj))
	)
       (role-rule obj role new-obj)
       (funcall next-function pred first new-obj)
       ))

(defun make-FAP (attr next-function pred first)
  #'(lambda (obj) (FAP attr next-function pred first (deref obj))))
(defun make-FRP (role next-function pred first)
  #'(lambda (obj) (FRP role next-function pred first (deref obj))))

