;************************************************************
;*                                                          *
;*                File : Predicate Manager
;*                                                          *
;************************************************************

;;; Compiler Instructions

(proclaim '(inline get-pred-of-int-repr get-nth-args set-nth-arg! exists-argument?
		   make-predicate get-pred-of-pred get-all-args-of-pred 
		   get-first-arg-of-pred))


;;; Representation of Predicate Restriction

(defun make-internal-repr-of-predicate (predicate)
  (let ((pred (make-array (1+ (get-arity-of-pred predicate))))
	)
       (setf (aref pred 0) predicate)
       pred))
  
(defun get-pred-of-int-repr (pred)
  (aref pred 0))

(defun get-nth-args (n pred)
  (aref pred n)
  )
(defun set-nth-arg! (pred n value)
  (setf (aref pred n) (cons value (aref pred n)))
  )
(defun exists-argument? (pred pos value)
  (member value (get-nth-args pos pred))
  )
(defun incomplete? (pred pos)
  (let ((arity (get-arity-of-pred (aref pred 0)))
	)
       (do ((c 1 (1+ c))
	    (result nil)
	    )
	   ((or result (> c arity)) result)
	   (or (= c pos)
	       (setq result (not (aref pred c))))
	   ))
  )

(defun all-arguments-exist? (pred)
  (let ((arity (get-arity-of-pred (aref pred 0)))
	)
       (dotimes (position arity t)
		(unless (aref pred (1+ position))
			(return nil))
		)))

(defun first-propagation? (pred position)
  (dotimes (pos (get-arity-of-pred (aref pred 0)) t)
	   (if (= pos position)
	       (if (get-nth-args pos pred)
		   (return nil))
	       (unless (get-nth-args pos pred)
		       (return nil)))))
(defun get-all-arguments (pred)
  (do ((arity (get-arity-of-pred (aref pred 0)))
       (c 1 (1+ c))
       (result nil)
       )
      ((> c arity) result)
      (setq result (append result (get-nth-args c pred)))
      ))

(defun make-dereferenced-matrix (pred pos value)
  (do ((counter (get-arity-of-pred (aref pred 0)) (1- counter))
       (result nil)
       )
      ((= counter 0) result)
      (setq result (cons (if (= counter pos)
			     (list (deref value)) 
			     (mapcar #'deref (get-nth-args counter pred)))
			 result))
      ))

(defun make-matrix (pred pos value)
  (do ((counter (get-arity-of-pred (aref pred 0)) (1- counter))
       (result nil)
       )
      ((= counter 0) result)
      (setq result (cons (if (= counter pos)
			     (list value)
			     (get-nth-args counter pred))
			 result))
      ))

;;; Representation of Predicates


(defun make-predicate (pred args)
  (cons pred args))
(defun get-pred-of-pred (pred)
  (car pred))
(defun get-all-args-of-pred (pred)
  (cdr pred))
(defun get-first-arg-of-pred (pred)
  (cadr pred))

