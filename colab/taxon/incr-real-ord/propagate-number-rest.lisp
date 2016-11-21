#|
******************************************************************************
	File 		: propagate-number-rest
	Last Change	: 13 nov 91
	Author		: dd
******************************************************************************
|#

(defun propagate-number-rest-> (number vector)
  (declare (special *index))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end))
      (if (set? counter vector)
	  (let ((obj (get-obj counter)))
	       (let ((lb (real-ord-obj-lb obj)))
		    (cond ((and lb
				(or (equal (list '> number) lb)
				    (> (nr-of-b lb) number)))
			   )
			  (t (set-lb (list '> number) obj)))))))
  )
(defun propagate-number-rest->= (number vector)
  (declare (special *index))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end))
      (if (set? counter vector)
	  (let ((obj (get-obj counter)))
	       (let ((lb (real-ord-obj-lb obj)))
		    (cond ((and lb
				(>= (nr-of-b lb) number)))
			  (t (set-lb (list '>= number) obj)))))))
  )
(defun propagate-number-rest-< (number vector)
  (declare (special *index))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end))
      (if (set? counter vector)
	  (let ((obj (get-obj counter)))
	       (let ((ub (real-ord-obj-ub obj)))
		    (cond ((and ub
				(or (equal (list '< number) ub)
				    (< (nr-of-b ub) number)))
			   )
			  (t (set-ub (list '< number) obj)))))))
  )
(defun propagate-number-rest-<= (number vector)
  (declare (special *index))
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= counter end))
      (if (set? counter vector)
	  (let ((obj (get-obj counter)))
	       (let ((ub (real-ord-obj-ub obj)))
		    (cond ((and ub
				(<= (nr-of-b ub) number)))
			  (t (set-ub (list '<= number) obj)))))))
  )
