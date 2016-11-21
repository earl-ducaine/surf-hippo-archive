;*******************************************************************************
;*
;*	Copy
;*
;*******************************************************************************

(defun copy-abstract-object (old)
  (let ((new (create-new-if-necessary old))
	)
       (setf (abstract-prim new) (abstract-prim old))
       (setf (abstract-neg-prim new) (abstract-neg-prim old))
       (setf (abstract-family new) (abstract-family old))
       (setf (abstract-neg-family new) (abstract-neg-family old))
       (setf (abstract-concept new) (abstract-concept old))
       (setf (abstract-neg-concept new) (abstract-neg-concept old))
       (setf (abstract-role-filler new)
	     (mapcar #'replace-old-fillers-by-new
		     (abstract-role-filler old)))
       (setf (abstract-attr-filler new)
	     (mapcar #'replace-old-fillers-by-new
		     (abstract-attr-filler old)))
