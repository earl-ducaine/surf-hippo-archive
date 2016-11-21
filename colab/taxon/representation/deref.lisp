;*******************************************************************************
;*
;*	Representation of Reference Chains
;*
;*******************************************************************************

(defun deref (obj)
  (if (or (eq 'NUNA (object-reference obj))
	  (eq 'UNA (object-reference obj)))
      obj
      (deref (object-reference obj))
  ))

(defun bind! (obj1 obj2)
  (trail! #'(lambda () (setf (object-reference obj1) 'NUNA)))
  (copy-trail! #'(lambda () (setf (object-reference (map-to-new-obj obj1)) 'NUNA)))
  (setf (object-reference obj1) obj2)
  (make-values obj1 obj2)
  )

(defun bind-abstract! (unk)
  (trail! #'(lambda () (setf (object-reference unk) 'NUNA)))
  (copy-trail! #'(lambda () (setf (object-reference (map-to-new-obj unk)) 'NUNA)))
  (setf (object-reference unk) 
	(make-abstract :id (get-id unk)
		       :reference (object-reference unk)))
  )

(defun bind-concrete! (unk domain)
  (trail! #'(lambda () (setf (object-reference unk) 'NUNA)))
  (copy-trail! #'(lambda () (setf (object-reference (map-to-new-obj unk)) 'NUNA)))
  (setf (object-reference unk) (make-concrete :id (get-id unk)
					      :domain domain))
  )

(defun bind-abstract->abstract! (obj1 obj2)
  (cond ((and (UNA? obj1) (UNA? obj2))
	 (clash 'bind-abstract->abstract! obj1 obj2))
	((or (UNA? obj1)
	     (new? obj2))
	 (bind! obj2 obj1))
	(t (bind! obj1 obj2))
	))

(defun bind-concrete->concrete! (obj1 obj2)
  (cond ((and (UNA? obj1) (UNA? obj2))
	 (clash 'bind-concrete->concrete! obj1 obj2))
	((or (UNA? obj1)
	     (new? obj2))
	 (bind! obj2 obj1))
	(t (bind! obj1 obj2))
	))

(defun bind-unknown->abstract! (unk abs)
  (cond ((UNA? unk)
	 (bind-abstract! unk)
	 (bind! abs unk))
	(t (bind! unk abs))
  ))

(defun bind-unknown->concrete! (unk concr)
  (cond ((UNA? unk)
	 (bind-concrete! unk (object-domain concr))
	 (bind! concr unk))
	(t (bind! unk concr))
	))

(defun bind-unknown->unknown! (unk1 unk2)
  (cond ((and (UNA? unk1) (UNA? unk2))
	 (clash))
	((or (UNA? unk1) (new? unk2))
	 (bind! unk2 unk1))
	(t (bind! unk1 unk2))
	))

(defun make-values (obj1 obj2)
  (list obj1 obj2))
(defun get-unbound (values)
  (cadr values))
(defun get-bound (values)
  (car values))

