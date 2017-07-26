;;; input functions to read in parameter files
;;;   created 7/91 - DLS

(defparameter *default-param-file-prefix*  (concatenate 'string *Surfdir* "/params/"))
(defparameter *param-filter* (concatenate 'string *Surfdir* "/turf/param2lisp"))

(defun load-sim (paramfile)
  (cond ((not (probe-file (concatenate 'string *default-param-file-prefix* paramfile ".params")))
		 (error-message "Parameter file \"~a\" does not exist" paramfile))
		(t (system (format nil "~a < ~a~a.params > ~a~a.lisp" *param-filter*
						   *default-param-file-prefix* paramfile
						   *default-param-file-prefix* paramfile))
		   (load (concatenate 'string *default-param-file-prefix* paramfile ".lisp")))))
		   
