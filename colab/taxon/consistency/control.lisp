
(defun satisfy-abox (abox)
  (initialize)
  (setq *main abox)
  (control_process)
)

(defun control_process ()
  (loop
   (let ((value (catch-clash (process_main_stack)))
	 )
	(cond ((clash? value)
	       (print-clash value)
	       (unless (backtracking!)
		       (return nil)))
	      ((process_or_stack) ;;; fails iff no further disjunction exists
	       )
	      (t (return t))
	      ))))

(defun compute-other-model ()
  (if (backtracking!)
      (control_process)))
