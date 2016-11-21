;;; file a-reasoning
;;; contents user commands for abox-reasoning
;;; 10-07-92 aa


#|
(defun check-abox  ()
  (clear-individual-table)
  (satisfy-abox (mapcar #'(lambda (item) (funcall (abox-entry-intern item))) (reverse *abox-table*)))
)
|#

(defun check-abox  ()
  (clear-individual-table)
  (satisfy-abox
   (mapcan #'(lambda (item)
		     (if (abox-entry-intern item)
			 (list (funcall (abox-entry-intern item)))
			 nil
			 ))
	   (reverse *abox-table*)))
)

