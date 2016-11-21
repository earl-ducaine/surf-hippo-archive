;*******************************************************************************
;*
;*	Help File -- not permanent
;*
;*******************************************************************************

(defun geto (item)
  (deref (gethash item *individual-table*)))
(defun getc (name)
  (get-cnf-of-conceptname name))

