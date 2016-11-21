;***************************************************************************
;***************************************************************************
;*
;*  Module           : 
;*  File             : 
;*  Author           : A. Abecker
;*  Edited           : 12.10.92
;*  Last change      : 
;*  Details changed  : function get-concrete-predicate-instantiation
;*
;*  Comment          : the file provides some functions of the TAXON-Toplevel
;*                     that are used to be access primitives in the context
;*                     of COLAB-interaction in particular between FORWARD and
;*                     TAXON
;*  Status           : NOT released
;*
;****************************************************************************

(defvar *individual-list* nil)

(defun finstances (concept)
  (mapcan #'(lambda (x) (and (finstance? x concept) 
			     (list x)))
	  *individual-list*))      ;;; mapcan as a filter - see STEELE p. 172


(defun get-concept-names () 
  ;; *hierarchy-generators)
;;*classify-list*)  new definition by DD 
(let ((result nil)
      )
  (maphash #'(lambda (k v) (if (or (eq (tbox-entry-type v) 'conc)
				   (eq (tbox-entry-type v) 'prim)
				   (eq (tbox-entry-type v) 'sprim)
				   )
			       (setf result 
				     (cons (tbox-entry-identifier v)
					   result))))
	   *tbox-table*)
  result))



(defun get-attr-names ()
  (let ((result nil))
       (maphash #'(lambda (x y) (if (attr-entry? y) (setf result (cons x result))))
		*tbox-table*)
       result))

(defun get-role-names ()
  (let ((result nil))
       (maphash #'(lambda (x y) (if (role-entry? y) (setf result (cons x result))))
		*tbox-table*)
       result))

(defun get-instance-names ()
  *individual-list*)

(defmacro make-indi (&rest idef)
  `(funcall #'fmake-indi ',idef))

(defun fmake-indi (i)
  (setf *individual-list*
	(union i *individual-list*))
  T)

(defun make-asse (a)
  (asse a))










